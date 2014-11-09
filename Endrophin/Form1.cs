using Endorphin.Instrument.PicoScope5000;
using Endorphin.Instrument.TwickenhamSmc;
using Endorphin.Experiment.MagnetRampAcquisition;
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
        MagnetController magnetController;
        MagnetRampAgent rampAgent;
        MagnetRampAcquisitionAgent rampAcquisitionAgent;
        DataPointCollection fieldTimePoints;
        DataPointCollection magneticFieldBins;

        public Form1()
        {
            InitializeComponent();
        }

        private void Form1_Load(object sender, EventArgs e)
        {
            scope = new PicoScope5000Agent(Resolution._12bit);

            var magnetControllerParameters = new DeviceParameters(
                zeroCurrentFieldInMillitesla: 144160.0,
                fieldCalibrationInMilliteslaPerAmp: -2.845,
                rampRateLimitInAmpsPerSec: 0.1,
                tripVoltageLimitInVolts: 2.5,
                maximumCurrentInAmps: 20.0,
                currentLimitInAmps: 5.0,
                shuntCalibrationInVoltsPerAmp: 19.949,
                outputResolutionInBits: 16);

            magnetController = new MagnetController("GPIB0::4", magnetControllerParameters);

            rampAgent = new MagnetRampAgent(magnetController);
            rampAcquisitionAgent = new MagnetRampAcquisitionAgent(scope, magnetController, rampAgent);

            fieldTimePoints = chart.Series[0].Points;
            magneticFieldBins = chart.Series[1].Points;
        }

        private void startStopButton_Click(object sender, EventArgs e)
        {
            fieldTimePoints.Clear();
            magneticFieldBins.Clear();

            var test = new Test(0, 256, 16, 02);

            var observables = rampAcquisitionAgent.Start(test);

            observables.fieldInMillitelsa.Subscribe(b => fieldTimePoints.Add(b));

            for (int i = 0; i < 256; i++)
            {
                magneticFieldBins.AddXY(i, 0);
                observables.pointCounts[i].Subscribe(c => magneticFieldBins[i].SetValueY(c));
            }
        }
    }
}
