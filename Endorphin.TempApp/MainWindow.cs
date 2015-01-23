using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using Endorphin.Instrument.PicoScope5000;
using Endorphin.Instrument.TwickenhamSmc;
using Endorphin.Experiment;
using Endorphin.Core.CSharpInterop;
using System.Threading;
using Microsoft.FSharp.Core;
using System.Reactive.Linq;
using System.IO;
using Newtonsoft.Json.Linq;

namespace Endorphin.TempApp
{
    public partial class MainWindow : Form
    {
        SynchronizationContext syncContext;

        MagnetControllerParameters magnetControllerParameters;
        MagnetControllerSession magnetControllerSession;
        PicoScope5000Session picoSession;
        MagnetController magnetController;
        PicoScope5000 pico;
        
        CwEprExperimentWorker experimentWorker;
        CwEprData experimentResult;

        private async Task<PicoScope5000> RequestPicoSessionControl()
        {
            var requestPicoControl = picoSession.RequestControlAsync().StartAsTask();
            var pico = await requestPicoControl;
            return pico;
        }

        private async Task<MagnetController> RequestMagnetControllerSessionControl()
        {
            var requestMagnetControllerControl = magnetControllerSession.RequestControlAsync().StartAsTask();
            var magnetController = await requestMagnetControllerControl;
            return magnetController;
        }

        private async Task PrepareAndStartExperiment()
        {
            pico = await RequestPicoSessionControl();
            magnetController = await RequestMagnetControllerSessionControl();

            var experiment = new CwEprExperiment(
                startingFieldIndex: magnetControllerParameters.IndexForField(Decimal.ToDouble(startingField.Value)),
                finalFieldIndex: magnetControllerParameters.IndexForField(Decimal.ToDouble(finalField.Value)),
                rampRateIndex: rampRate.SelectedIndex,
                returnToZero: returnToZero.Checked,
                conversionTime: Decimal.ToInt32(sampleInterval.Value),
                downsampling: downsamplingEnabled.Checked
                    ? FSharpOption<UInt32>.Some(Decimal.ToUInt32(downsamplingRatio.Value))
                    : FSharpOption<UInt32>.None,
                quadratureDetection: quadratureDetection.Checked,
                numberOfScans: Decimal.ToInt32(numberOfScans.Value));


            experimentWorker = new CwEprExperimentWorker(experiment, magnetController, pico);
            experimentResult = null;

            experimentWorker.PrepareAndStart();
        }

        private void SetWorkerCallbacks()
        {
            experimentWorker.Canceled.AddHandler((sender, args) =>
                {
                    SetUiFinished("canceled");
                    ReleaseInstruments();
                });

            experimentWorker.Error.AddHandler((sender, args) =>
                {
                    SetUiFinished("failed");
                    ReleaseInstruments();
                });

            experimentWorker.ScanFinished.AddHandler((sender, scan) =>
                {
                    SetResultText(string.Format("finished scan {0} of {1}", scan, experimentWorker.Experiment.numberOfScans));
                });

            experimentWorker.StoppedAfterScan.AddHandler((sender, scan) =>
                {
                    SetResultText(string.Format("stopped after {0} scans", scan));
                });

            experimentWorker.Success.AddHandler((sender, args) =>
                {
                    SetUiFinished("finished experiment");
                    ReleaseInstruments();
                });

            var canceledOrFinished =
                experimentWorker.Canceled
                    .Select(new Func<OperationCanceledException, Unit>(ex => null))
                    .Amb(experimentWorker.Success);

            experimentWorker.DataUpdated
                .TakeUntil(canceledOrFinished)
                .LastAsync()
                .Subscribe((data) => experimentResult = data);
        }

        private void ReleaseInstruments()
        {
            (experimentWorker.PicoScope as IDisposable).Dispose();
            (experimentWorker.MagnetController as IDisposable).Dispose();
        }

        private void SetResultText(string resultText) {
            resultsGroup.Text = string.Format("CW EPR Experiment ({0})", resultText);
            resultsGroup.Update();
        }

        private void SetUiRunning()
        {
            SetResultText("running");

            startingField.Enabled = false;
            finalField.Enabled = false;
            magnetControllerSteps.Enabled = false;
            rampRate.Enabled = false;
            returnToZero.Enabled = false;
            numberOfScans.Enabled = false;
            sampleInterval.Enabled = false;
            downsamplingEnabled.Enabled = false;
            downsamplingRatio.Enabled = false;
            quadratureDetection.Enabled = false;

            prepareStartButton.Enabled = false;
            stopButton.Enabled = true;
            stopToZero.Enabled = true;
            stopAfterScan.Enabled = true;

            menuSave.Enabled = false;

            var liveChart = experimentWorker.LiveChart(TimeSpan.FromSeconds(1));
            resultsGroup.Controls.Clear();
            resultsGroup.Controls.Add(liveChart);
            liveChart.Dock = DockStyle.Fill;
        }

        private void SetUiFinished(string resultText)
        {
            SetResultText(resultText);

            startingField.Enabled = true;
            finalField.Enabled = true;
            magnetControllerSteps.Enabled = true;
            rampRate.Enabled = true;
            returnToZero.Enabled = true;
            numberOfScans.Enabled = true;
            sampleInterval.Enabled = true;
            downsamplingEnabled.Enabled = true;
            downsamplingRatio.Enabled = downsamplingEnabled.Checked;
            quadratureDetection.Enabled = true;

            prepareStartButton.Enabled = true;
            stopButton.Enabled = false;
            stopAfterScan.Enabled = false;
            stopToZero.Enabled = false;

            menuSave.Enabled = true;
        }

        public MainWindow()
        {
            InitializeComponent();
            syncContext = SynchronizationContext.Current;
        }

        private void MainWindow_Load(object sender, EventArgs e)
        {
            magnetControllerParameters = new MagnetControllerParameters(
                staticField: 14.146, // tesla
                fieldCalibration: -0.002845, // tesla per amp
                rampRateLimit: 0.1, // amps per second
                tripVoltageLimit: 2.5, // volts
                maximumCurrent: 20.0, // amps
                currentLimit: 17.5, // amps
                shuntOffset: 0.002, // volts
                shuntCalibration: 0.400, // volts per amp
                shuntNoise: 0.100, // volts
                outputResolutionInBits: 16,
                setPointDecimalPlaces: 4,
                calibratedRampRates: new double[] {
                    0.00020, 0.00024, 0.00026, 0.00030, 0.00036, 0.00042, 0.00048, 0.00054, 
                    0.00064, 0.00072, 0.00084, 0.00098, 0.00110, 0.00130, 0.00150, 0.00170,
                    0.0020, 0.0024, 0.0026, 0.0030, 0.0036, 0.0042, 0.0048, 0.0054, 
                    0.0064, 0.0072, 0.0084, 0.0098, 0.0110, 0.0130, 0.0150, 0.0170,
                    0.020, 0.024, 0.026, 0.030, 0.036, 0.042, 0.048, 0.054, 
                    0.064, 0.072, 0.084, 0.098, 0.110, 0.130, 0.150, 0.170,
                    0.20, 0.24, 0.26, 0.30, 0.36, 0.42, 0.48, 0.54, 
                    0.64, 0.72, 0.84, 0.98, 1.10, 1.30, 1.50, 1.70, 
                    2.0 });

            foreach (var availableRampRate in magnetControllerParameters.AvailableCurrentRampRates) {
                rampRate.Items.Add(availableRampRate);
            }
            rampRate.SelectedIndex = 32;

            startingField.Minimum = new Decimal(magnetControllerParameters.MinimumField);
            startingField.Maximum = new Decimal(magnetControllerParameters.MaximumField);
            startingField.Value = new Decimal(magnetControllerParameters.staticField);
            startingField.Increment = new Decimal(magnetControllerParameters.FieldStep);

            finalField.Minimum = new Decimal(magnetControllerParameters.MinimumField);
            finalField.Maximum = new Decimal(magnetControllerParameters.MaximumField);
            finalField.Value = new Decimal(magnetControllerParameters.FieldForIndex(1023));
            finalField.Increment = new Decimal(magnetControllerParameters.FieldStep);

            magnetControllerSteps.Minimum = new Decimal(- 2 * (magnetControllerParameters.NumberOfCurrentSteps - 1));
            magnetControllerSteps.Maximum = new Decimal(+ 2 * (magnetControllerParameters.NumberOfCurrentSteps - 1));
            magnetControllerSteps.Value = new Decimal(1023);

            currentRangeLabel.Text =
                string.Format("Magnet controller current range: {0:0.0000} to {1:0.0000} A.",
                    magnetControllerParameters.CurrentForIndex(0),
                    magnetControllerParameters.CurrentForIndex(1023));

            estimatedTimeLabel.Text =
                string.Format("Estimated duration: {0:0.0} s.",
                    (Decimal.ToInt32(numberOfScans.Value)
                        * Math.Abs(magnetControllerParameters.CurrentForIndex(1023) - magnetControllerParameters.CurrentForIndex(0))
                        / magnetControllerParameters.CurrentRampRateForIndex(rampRate.SelectedIndex)));

            startingField.ValueChanged += this.fieldRange_ValueChanged;
            finalField.ValueChanged += this.fieldRange_ValueChanged;
            magnetControllerSteps.ValueChanged += this.fieldRange_ValueChanged;

            picoSession = new PicoScope5000Session("CW336/061", Resolution._14bit);
            magnetControllerSession = new MagnetControllerSession("GPIB0::4", magnetControllerParameters);
        }

        private void fieldRange_ValueChanged(object sender, EventArgs e)
        {
            startingField.ValueChanged -= this.fieldRange_ValueChanged;
            finalField.ValueChanged -= this.fieldRange_ValueChanged;
            magnetControllerSteps.ValueChanged -= this.fieldRange_ValueChanged;

            var startingFieldIndex = magnetControllerParameters.IndexForField(Decimal.ToDouble(startingField.Value));
            var finalFieldIndex = 
                sender == magnetControllerSteps
                ? startingFieldIndex + Decimal.ToInt32(magnetControllerSteps.Value)
                : magnetControllerParameters.IndexForField(Decimal.ToDouble(finalField.Value));
            
            startingField.Value = new Decimal(magnetControllerParameters.FieldForIndex(startingFieldIndex));
            finalField.Value = new Decimal(magnetControllerParameters.FieldForIndex(finalFieldIndex));
            magnetControllerSteps.Value = new Decimal(finalFieldIndex - startingFieldIndex);

            currentRangeLabel.Text =
                string.Format("Magnet controller current range: {0:0.0000} to {1:0.0000} A.",
                    magnetControllerParameters.CurrentForIndex(startingFieldIndex),
                    magnetControllerParameters.CurrentForIndex(finalFieldIndex));
            
            estimatedTimeLabel.Text =
                string.Format("Estimated duration: {0:0.0} s.",
                    (Decimal.ToInt32(numberOfScans.Value)
                        * Math.Abs(magnetControllerParameters.CurrentForIndex(1023) - magnetControllerParameters.CurrentForIndex(0))
                        / magnetControllerParameters.CurrentRampRateForIndex(rampRate.SelectedIndex)));

            startingField.ValueChanged += this.fieldRange_ValueChanged;
            finalField.ValueChanged += this.fieldRange_ValueChanged;
            magnetControllerSteps.ValueChanged += this.fieldRange_ValueChanged;
        }

        private void rampRate_SelectedIndexChanged(object sender, EventArgs e)
        {
            estimatedTimeLabel.Text =
                string.Format("Estimated duration: {0:0.0} s.",
                    (Decimal.ToInt32(numberOfScans.Value)
                        * Math.Abs(magnetControllerParameters.CurrentForIndex(1023) - magnetControllerParameters.CurrentForIndex(0))
                        / magnetControllerParameters.CurrentRampRateForIndex(rampRate.SelectedIndex)));
        }

        private void downsamplingEnabled_CheckedChanged(object sender, EventArgs e)
        {
            if (downsamplingEnabled.Checked)
            {
                downsamplingRatio.Enabled = true;
            }
            else
            {
                downsamplingRatio.Enabled = false;
                downsamplingRatio.Value = new Decimal(1.0);
            }
        }

        private async void prepareStartButton_Click(object sender, EventArgs e)
        {
            if (magnetControllerSteps.Value == new Decimal(0.0))
            {
                MessageBox.Show("Must have a non-zero number of magnet controller steps.");
                return;
            }
            await PrepareAndStartExperiment();
            SetWorkerCallbacks();
            SetUiRunning();
        }

        private void stopButton_Click(object sender, EventArgs e)
        {
            experimentWorker.Cancel(new RampCancellationOptions(false));
        }

        private void stopToZero_Click(object sender, EventArgs e)
        {
            experimentWorker.Cancel(new RampCancellationOptions(true));
        }

        private void stopAfterScan_Click(object sender, EventArgs e)
        {
            experimentWorker.StopAfterScan();
        }

        private void menuSave_Click(object sender, EventArgs e)
        {
            SaveFileDialog saveDialog = new SaveFileDialog();
            saveDialog.Filter = "Comma-separated value files (*.csv)|*.csv";
            saveDialog.RestoreDirectory = true;

            if (saveDialog.ShowDialog() == DialogResult.OK)
            {
                Stream dataFileStream;
                if ((dataFileStream = saveDialog.OpenFile()) != null)
                {
                    using (var writer = new StreamWriter(dataFileStream))
                    {
                        foreach (var line in experimentResult.CsvData())
                        {
                            writer.WriteLine(line);
                        }
                    }
                    dataFileStream.Close();
                }

                Stream descriptionFileStream;
                if ((descriptionFileStream = File.Open(Path.ChangeExtension(saveDialog.FileName, "txt"), FileMode.Create)) != null)
                {
                    using (var writer = new StreamWriter(descriptionFileStream))
                    {
                        var json = JObject.FromObject(new
                            {
                                experimentParameters = experimentWorker.Experiment,
                                sampleNotes = sampleNotes.Text,
                                experimentNotes = experimentNotes.Text
                            });
                        writer.Write(json.ToString());
                    }
                }

            }
        }
    }
}
