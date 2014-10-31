using Endorphin.Instrument.PicoScope5000;
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Diagnostics;
using System.Drawing;
using System.Linq;
using System.Reactive.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using System.Windows.Forms;
using System.Windows.Forms.DataVisualization.Charting;

namespace Endrophin.UI
{
    public partial class Form1 : Form
    {
        PicoScope5000Agent scope;
        StreamAgent streamAgent;
        Func<short, double> adcToVols;
        DataPointCollection dataPoints;

        public Form1()
        {
            InitializeComponent();
        }

        private void Form1_Load(object sender, EventArgs e)
        {
            scope = new PicoScope5000Agent(Resolution._12bit);
            dataPoints = chart.Series[0].Points;
            var inputRange = Range._1V;

            var enabledChannelSettings = new ChannelSettings(true, Coupling.DC, inputRange, 0.0);
            var disabledChannelSettings = new ChannelSettings(false, Coupling.DC, inputRange, 0.0);
            scope.SetChannelSettings(Channel.A, enabledChannelSettings);
            scope.SetChannelSettings(Channel.B, enabledChannelSettings);
            scope.SetChannelSettings(Channel.C, disabledChannelSettings);
            scope.SetChannelSettings(Channel.D, disabledChannelSettings);
            scope.SetAutoTrigger(1);

            adcToVols = scope.GetAdcCountToVoltsConversion(inputRange, 0.0);

            chart.ChartAreas[0].AxisX.Minimum = -1.1 * inputRange.ToVolts();
            chart.ChartAreas[0].AxisX.Maximum = +1.1 * inputRange.ToVolts();
            chart.ChartAreas[0].AxisY.Minimum = -1.1 * inputRange.ToVolts();
            chart.ChartAreas[0].AxisY.Maximum = +1.1 * inputRange.ToVolts();
        }

        private void startStopButton_Click(object sender, EventArgs e)
        {
            if (streamAgent == null)
            {
                dataPoints.Clear();

                streamAgent = scope.CreateStreamAgent();

                var x = 
                    from channelData in streamAgent.Observe(Channel.A, Downsampling.None)
                    from sample in channelData.samples
                    select adcToVols(sample);

                var y = 
                    from channelData in streamAgent.Observe(Channel.B, Downsampling.None)
                    from sample in channelData.samples
                    select adcToVols(sample);

                var xy = Observable.Zip(x, y);

                xy.ObserveOn(SynchronizationContext.Current)
                  .Subscribe(
                    xysamples => {
                        dataPoints.AddXY(xysamples[0], xysamples[1]);
                        if (dataPoints.Count > 10000)
                            dataPoints.RemoveAt(0);
                    });
                
                var streamingParameters = new StreamingParmaeters(
                    sampleInterval: SampleInterval.FromMicroseconds(100),
                    downsamplingRatio: 1, 
                    maximumPreTriggerSamples: 0, 
                    maximumPostTriggerSamples: 10000, 
                    autoStop: false);

                var streamStatus = streamAgent.RunStream(streamingParameters);
                streamStatus.ObserveOn(SynchronizationContext.Current)
                            .Subscribe(status => statusLabel.Text = String.Format("Stream status: {0}", status));
            }
            else
            {
                streamAgent.StopStream();
                (streamAgent as IDisposable).Dispose();
                streamAgent = null;
            }
        }
    }
}
