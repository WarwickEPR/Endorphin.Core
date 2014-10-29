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
        int chartIndex = 0;

        public Form1()
        {
            InitializeComponent();
        }

        private void Form1_Load(object sender, EventArgs e)
        {
            scope = new PicoScope5000Agent(Resolution._12bit);
            var inputRange = Range._1V;

            var enabledChannelSettings = new ChannelSettings(true, Coupling.DC, inputRange, 0.0);
            var disabledChannelSettings = new ChannelSettings(false, Coupling.DC, inputRange, 0.0);
            scope.SetChannelSettings(Channel.A, enabledChannelSettings);
            scope.SetChannelSettings(Channel.B, enabledChannelSettings);
            scope.SetChannelSettings(Channel.C, disabledChannelSettings);
            scope.SetChannelSettings(Channel.D, disabledChannelSettings);
            scope.SetAutoTrigger(1);

            adcToVols = scope.GetAdcCountToVoltsConversion(inputRange, 0.0);

            chart.ChartAreas[0].AxisX.Minimum = -1.2 * inputRange.ToVolts();
            chart.ChartAreas[0].AxisX.Maximum = +1.2 * inputRange.ToVolts();
            chart.ChartAreas[0].AxisY.Minimum = -1.2 * inputRange.ToVolts();
            chart.ChartAreas[0].AxisY.Maximum = +1.2 * inputRange.ToVolts();
        }

        private void startStopButton_Click(object sender, EventArgs e)
        {
            if (streamAgent == null)
            {
                chart.Series[0].Points.Clear();

                streamAgent = scope.CreateStreamAgent();

                var x = from channelData in streamAgent.Observe(Channel.A, Downsampling.None)
                        select Array.ConvertAll(channelData.samples, value => adcToVols(value));

                var y = from channelData in streamAgent.Observe(Channel.B, Downsampling.None)
                        select Array.ConvertAll(channelData.samples, value => adcToVols(value));

                var xy = Observable.Zip(x, y);

                xy.ObserveOn(SynchronizationContext.Current)
                  .Subscribe(
                    values => {
                        var length = values[0].Length;
                        chart.Series[0].Points.SuspendUpdates();
                        chartIndex += length;

                        for (var i = 0; i < length; i++)
                            chart.Series[0].Points.AddXY(values[0][i], values[1][i]);
                        while (chart.Series[0].Points.Count > 10000)
                            chart.Series[0].Points.RemoveAt(0);

                        chart.Series[0].Points.ResumeUpdates();
                        chart.Series[0].Points.Invalidate();
                    });

                var streamingParameters = new StreamingParmaeters(
                    StreamingInterval.NewMicroseconds(100), 1, 0, 10000, false);

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
