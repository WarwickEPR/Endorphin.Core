using Endorphin.Instrument.PicoScope5000;
using Endorphin.Core.CSharpInterop;
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Reactive.Linq;
using System.Threading;
using System.Threading.Tasks;
using System.Windows.Forms;
using Microsoft.FSharp.Core;
using Microsoft.FSharp.Collections;
using FSharp.Charting;

namespace StreamingScope
{
    public partial class MainWindow : Form
    {
        // this manages the connection to the PicoScope hardware
        PicoScope5000Session picoSession = new PicoScope5000Session(Resolution._12bit);

        public MainWindow()
        {
            InitializeComponent();
        }

        // when the main window loads, first establish a connection to the hardware before loading the form
        private async void MainWindow_Load(object sender, EventArgs e)
        {
            await picoSession.ConnectAsync().StartAsTask();
            startButton.Enabled = true;
        }

        // this map specifies the list of active channels along with their respective input settings and list of downsampling modes...
        // each channel can have its own settings, although there is only one in this example
        private FSharpMap<Channel, ChannelStream> ActiveChannels
        {
            get
            {
                var inputSettings = new InputSettings(
                    coupling: Coupling.DC,
                    range: Range._200mV,
                    analogueOffset: 0.0, // volts 
                    bandwidthLimit: BandwidthLimit._20MHz);

                var downsamplingModes =
                    new FSharpSet<DownsamplingMode>(new[] { 
                        DownsamplingMode.Aggregate, 
                        DownsamplingMode.Averaged 
                    });

                return new FSharpMap<Channel, ChannelStream>(new[] { 
            // add more channels with their respective settings to enable them here.
            // note that combining DownsamplingMode.None with other downsampling modes will result in an error
                    Tuple.Create(Channel.A, new ChannelStream(inputSettings, downsamplingModes))
                });
            }
        }

        // this specifies all the streaming parameters for the acquisition
        private PicoScope5000Stream StreamAcquisition
        {
            get
            {
                return new PicoScope5000Stream(
                    sampleInterval: (int)1e5, // nanoseconds

                    // this specifies the downsampling ratio, if there is one: use Stream.NoDownsampling or .DownsamplingWithRatio(ratio).
                    // note that if you specify .NoDownsampling and have a stream with a DownsamplingMode other than None in the active channels,
                    // you will get an error; conversely, if you are using downsampling, you must specify a .DownsamplingWithRatio(ratio).
                    downsamplingRatio: PicoScope5000Stream.DownsamplingWithRatio(2000),

                    // this specifies whether the stream will stop manually or automatically after a specified number of samples: use
                    // StreamStop.Manual to stop the stream manually or StreamStop.Auto(preTriggerSamples, postTriggerSamples) to stop
                    // the stream automatically
                    streamStop: StreamStop.Manual,

                    // this specifies how the stream will be triggered: use TriggerSettings.Auto(delay) to start after the specified delay
                    // in milliseconds and TriggerSettings.Simple(simpleTriggerSettings) to specify a single channel trigger
                    triggerSettings: TriggerSettings.Auto(1),

                    activeChannels: ActiveChannels, // active channels, as defined above
                    memorySegment: 0); // memory segment
            }
        }

        // this defines how the start button click event is handled
        private async void startButton_Click(object sender, EventArgs e)
        {
            // disable the start button until the user clicks this acquisition finishes
            startButton.Enabled = false;

            // take control of the PicoScope device
            var requestPicoControl = picoSession.RequestControlAsync().StartAsTask();
            using (var pico = await requestPicoControl)
            {
                // create a stream worker which will execute the speified stream on the hardare
                var streamWorker = new StreamWorker(pico, StreamAcquisition);

                // chart all channels and respective buffers sampled by the stream worker and update every second
                var chart = streamWorker.LiveChartAllStreams(refreshInterval: TimeSpan.FromSeconds(1), showNames: true);

                // replace the chart control in the chartPanel on the UI with one which will plot data for this streamWorker
                chartPanel.Controls.Clear();

                var chartControl = new ChartTypes.ChartControl(chart);
                chartControl.Dock = DockStyle.Fill;
                chartPanel.Controls.Add(chartControl);

                // run the stream worker
                streamWorker.PrepareAndStart();

                // enable the stop button and wait for the stream to finish (either automatically or manually)
                using (stopButton.GetClickObservable().FirstAsync().Subscribe(
                    stopArgs => streamWorker.Stop())) // stop the stream when the button is clicked
                {
                    stopButton.Enabled = true;

                    await streamWorker.StatusChanged.LastAsync();
                    stopButton.Enabled = false;
                }
            }

            // re-enable the start button
            startButton.Enabled = true;
        }

        // when the form closes, close the connection to the PicoScope
        private async void MainWindow_FormClosing(object sender, FormClosingEventArgs e)
        {
            await picoSession.CloseSessionAsync().StartAsTask();
        }
    }
}