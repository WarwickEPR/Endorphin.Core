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
        PicoScope5000Session picoSession; // this manages the connection to the hardware (via ps5000aOpen/CloseUnit)
        PicoScope5000 pico; // this object is obtained from the session by requesting control of the hardware and sends commands to the hardware
        StreamWorker streamWorker; // this object can use a supplied PicoScope5000 object and some specified streaming parameters to run a general streaming acquisition

        public MainWindow()
        {
            InitializeComponent();
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
                    new Tuple<Channel, ChannelStream>(Channel.A, new ChannelStream(inputSettings, downsamplingModes))
                });
            }
        }

        // this specifies all the streaming parameters for the acquisition
        private PicoScope5000Stream StreamAcquisition
        {
            get
            {
                return new PicoScope5000Stream(
                    sampleInterval: (int) 1e5, // nanoseconds

                    // this specifies the downsampling ratio, if there is one: use Stream.NoDownsampling or .DownsamplingRatio(ratio).
                    // note that if you specify .NoDownsampling and have a stream with a DownsamplingMode other than None in the active channels,
                    // you will get an error; conversely, if you are using downsampling, you must specify a .DownsamplingRatio(ratio).
                    downsampling: PicoScope5000Stream.DownsamplingRatio(2000),

                    // manual or auto stop
                    streamStop: StreamStop.Manual, 

                    // auto trigger or "simple" trigger... advanced trigger settings are not implemented
                    // auto trigger requires a specified delay in milliseconds
                    triggerSettings: TriggerSettings.Auto(1), 

                    activeChannels: ActiveChannels, // active channels, as defined above
                    memorySegment: 0); // memory segment
            }
        }

        private async void MainWindow_Load(object sender, EventArgs e)
        {  
            // when the form loads, create a connection to the hardware and take control of the device 
            picoSession = new PicoScope5000Session(Resolution._12bit); // set resolution and specify a device serial by changing the constructor arguments if needed
            var requestPicoControl = picoSession.RequestControlAsync().StartAsTask();
            pico = await requestPicoControl;
        }

        private void MainWindow_FormClosing(object sender, FormClosingEventArgs e)
        {
            // when the form closes, close the connection the hardware
            (pico as IDisposable).Dispose();
            picoSession.CloseSessionAsync().RunSynchronously();
        }

        private void startButton_Click(object sender, EventArgs e)
        {
            if (streamWorker == null)
            {
                // create a stream worker which will execute the speified stream on the hardare
                streamWorker = new StreamWorker(pico, StreamAcquisition);

                // chart all channels and respective buffers sampled by the stream worker and update every second
                var chart = streamWorker.ChartAll(refreshInterval: TimeSpan.FromSeconds(1));

                // replace the chart control in the chartPanel on the UI with one which will plot data for this streamWorker
                chartPanel.Controls.Clear();
                
                var chartControl = new ChartTypes.ChartControl(chart);
                chartControl.Dock = DockStyle.Fill;
                chartPanel.Controls.Add(chartControl);
                
                // run the stream worker
                streamWorker.PrepareAndStart();
            }
        }

        private void stopButton_Click(object sender, EventArgs e)
        {
            if (streamWorker != null)
            {
                // stop and discard the streamWorker
                streamWorker.Stop();
                streamWorker = null;
            }
        }
    }
}
