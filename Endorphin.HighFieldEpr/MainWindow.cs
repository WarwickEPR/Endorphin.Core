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
using FSharp.Charting;
using System.Reactive.Disposables;

namespace Endorphin.TempApp
{
    public partial class MainWindow : Form
    {
        // TODO: move this into a configuration file

        // create a session for connecting to the magnet controller
        MagnetControllerSession magnetControllerSession = 
            new MagnetControllerSession("GPIB0::4", // VISA address
                new MagnetControllerParameters(
                    staticField: 14.146, // tesla
                    fieldCalibration: -0.002845, // tesla per amp
                    rampRateLimit: 0.1, // amps per second
                    tripVoltageLimit: 2.5, // volts
                    maximumCurrent: 20.0, // amps
                    currentLimit: 17.5, // amps
                    shuntOffset: 0.002, // volts
                    shuntCalibration: 0.400, // volts per amp
                    shuntNoise: 0.100, // volts
                    outputResolution: 16, // bits
                    calibratedRampRates: new double[] { // amps per sec
                        0.00020, 0.00024, 0.00026, 0.00030, 0.00036, 0.00042, 0.00048, 0.00054, 
                        0.00064, 0.00072, 0.00084, 0.00098, 0.00110, 0.00130, 0.00150, 0.00170,
                        0.0020, 0.0024, 0.0026, 0.0030, 0.0036, 0.0042, 0.0048, 0.0054, 
                        0.0064, 0.0072, 0.0084, 0.0098, 0.0110, 0.0130, 0.0150, 0.0170,
                        0.020, 0.024, 0.026, 0.030, 0.036, 0.042, 0.048, 0.054, 
                        0.064, 0.072, 0.084, 0.098, 0.110, 0.130, 0.150, 0.170,
                        0.20, 0.24, 0.26, 0.30, 0.36, 0.42, 0.48, 0.54, 
                        0.64, 0.72, 0.84, 0.98, 1.10, 1.30, 1.50, 1.70, 
                        2.0 }));

        // create a session for connecting to the PicoScope
        PicoScope5000Session picoSession =
            new PicoScope5000Session("CW336/061", Resolution._14bit); // device serial and vertical resolution

        public MainWindow()
        {
            InitializeComponent();
        }

        // when the window loads, connect to the hardware and set up the UI with some default values
        private async void MainWindow_Load(object sender, EventArgs e)
        {
            await magnetControllerSession.ConnectAsync().StartAsTask();
            await picoSession.ConnectAsync().StartAsTask();

            var magnetControllerParameters = magnetControllerSession.MagnetControllerParameters;

            foreach (var availableRampRate in magnetControllerParameters.AvailableCurrentRampRates)
            {
                rampRate.Items.Add(availableRampRate);
            }
            rampRate.SelectedIndex = 32; 

            startingField.Minimum = new Decimal(magnetControllerParameters.MinimumField);
            startingField.Maximum = new Decimal(magnetControllerParameters.MaximumField);
            startingField.Value = new Decimal(magnetControllerParameters.StaticField);
            startingField.Increment = new Decimal(magnetControllerParameters.FieldStep);

            finalField.Minimum = new Decimal(magnetControllerParameters.MinimumField);
            finalField.Maximum = new Decimal(magnetControllerParameters.MaximumField);
            finalField.Value = new Decimal(magnetControllerParameters.FieldForIndex(1023));
            finalField.Increment = new Decimal(magnetControllerParameters.FieldStep);

            magnetControllerSteps.Minimum = new Decimal(-2 * (magnetControllerParameters.NumberOfCurrentSteps - 1));
            magnetControllerSteps.Maximum = new Decimal(+2 * (magnetControllerParameters.NumberOfCurrentSteps - 1));
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
        }

        // subscribe to the experiment worker observables and set stop button click handlers 
        private IDisposable SubscribeToExperimentObservables(CwEprExperimentWorker experimentWorker)
        {
            var statusChangedSub = experimentWorker.StatusChanged.ObserveOn(SynchronizationContext.Current)
                .Subscribe(status =>
                {
                    resultsGroup.Text = string.Format("CW EPR Experiment ({0})", status.MessageString());
                    resultsGroup.Update();
                });

            var stopButtonSub = stopButton.GetClickObservable().Subscribe(args => experimentWorker.Cancel(new RampCancellationOptions(false)));
            var stopToZeroSub = stopToZero.GetClickObservable().Subscribe(args => experimentWorker.Cancel(new RampCancellationOptions(true)));
            var stopAfterScanSub = stopAfterScan.GetClickObservable().Subscribe(args => experimentWorker.StopAfterScan());

            return Disposable.Create(() =>
                {
                    statusChangedSub.Dispose();
                    stopButtonSub.Dispose();
                    stopToZeroSub.Dispose();
                    stopAfterScanSub.Dispose();
                });
        }
        
        // create a chart control for the experiment worker data and put it in the resultsGroup panel
        private void SetChart(CwEprExperimentWorker experimentWorker)
        {
            var liveChart = new ChartTypes.ChartControl(experimentWorker.LiveChartExperiment(TimeSpan.FromSeconds(1)));
            resultsGroup.Controls.Clear();
            resultsGroup.Controls.Add(liveChart);
            liveChart.Dock = DockStyle.Fill;
        }

        // set UI controls for a running experiments
        private void SetUiRunning()
        {
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
        }

        // set UI controls for a finished experiment
        private void SetUiFinished()
        {
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

        // handle a change to one of the magnetic field range inputs
        private void fieldRange_ValueChanged(object sender, EventArgs e)
        {
            // remove the other control event handlers temporarily to avoid triggering them
            startingField.ValueChanged -= this.fieldRange_ValueChanged;
            finalField.ValueChanged -= this.fieldRange_ValueChanged;
            magnetControllerSteps.ValueChanged -= this.fieldRange_ValueChanged;

            var smcParams = magnetControllerSession.MagnetControllerParameters;

            var startingFieldIndex = smcParams.OutputIndexForField(Decimal.ToDouble(startingField.Value));
            var finalFieldIndex = 
                sender == magnetControllerSteps
                ? startingFieldIndex + Decimal.ToInt32(magnetControllerSteps.Value)
                : smcParams.OutputIndexForField(Decimal.ToDouble(finalField.Value));
            
            startingField.Value = new Decimal(smcParams.FieldForIndex(startingFieldIndex));
            finalField.Value = new Decimal(smcParams.FieldForIndex(finalFieldIndex));
            magnetControllerSteps.Value = new Decimal(finalFieldIndex - startingFieldIndex);

            currentRangeLabel.Text =
                string.Format("Magnet controller current range: {0:0.0000} to {1:0.0000} A.",
                    smcParams.CurrentForIndex(startingFieldIndex),
                    smcParams.CurrentForIndex(finalFieldIndex));
            
            // update the experiment duration estimate
            estimatedTimeLabel.Text =
                string.Format("Estimated duration: {0:0.0} s.",
                    (Decimal.ToInt32(numberOfScans.Value)
                        * Math.Abs(smcParams.FieldForIndex(finalFieldIndex) - smcParams.FieldForIndex(startingFieldIndex))
                        / smcParams.FieldRampRateForIndex(rampRate.SelectedIndex)));

            // re-add the control event handlers again
            startingField.ValueChanged += this.fieldRange_ValueChanged;
            finalField.ValueChanged += this.fieldRange_ValueChanged;
            magnetControllerSteps.ValueChanged += this.fieldRange_ValueChanged;
        }

        // when the ramp rate is changed, update the duration estimate
        private void rampRate_SelectedIndexChanged(object sender, EventArgs e)
        {
            var smcParams = magnetControllerSession.MagnetControllerParameters;
            estimatedTimeLabel.Text =
                string.Format("Estimated duration: {0:0.0} s.",
                    (Decimal.ToInt32(numberOfScans.Value)
                        * Math.Abs(smcParams.CurrentForIndex(1023) - smcParams.CurrentForIndex(0))
                        / smcParams.CurrentRampRateForIndex(rampRate.SelectedIndex)));
        }

        // when the downsampling is disabled, also disable the downsampling ratio input and enable it otherwise
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
        
        // handle the start button  click
        private async void prepareStartButton_Click(object sender, EventArgs e)
        {
            if (magnetControllerSteps.Value == new Decimal(0.0))
            {
                MessageBox.Show("Must have a non-zero number of magnet controller steps.");
                return;
            }
            
            // take control of the hardware
            var magnetControllerParameters = magnetControllerSession.MagnetControllerParameters;
            using (var pico = await picoSession.RequestControlAsync().StartAsTask())
            using (var magnetController = await magnetControllerSession.RequestControlAsync().StartAsTask())
            {
                // set up the experiment according to the parameters entered on the UI
                var experiment = new CwEprExperiment(
                    startingFieldIndex: magnetControllerParameters.OutputIndexForField(Decimal.ToDouble(startingField.Value)),
                    finalFieldIndex: magnetControllerParameters.OutputIndexForField(Decimal.ToDouble(finalField.Value)),
                    rampRateIndex: rampRate.SelectedIndex,
                    returnToZero: returnToZero.Checked,
                    sampleInterval: Decimal.ToInt32(sampleInterval.Value),
                    downsamplingRatio: downsamplingEnabled.Checked
                        ? FSharpOption<UInt32>.Some(Decimal.ToUInt32(downsamplingRatio.Value))
                        : FSharpOption<UInt32>.None,
                    quadratureDetection: quadratureDetection.Checked,
                    numberOfScans: Decimal.ToInt32(numberOfScans.Value));

                // create the experiment worker and subscribe to its events
                var experimentWorker = new CwEprExperimentWorker(experiment, magnetController, pico);
                using (var workerSubs = SubscribeToExperimentObservables(experimentWorker))
                {
                    // disable the UI controls until the experiment finishes and update the chart 
                    SetUiRunning();
                    SetChart(experimentWorker);
                    experimentWorker.PrepareAndStart(); // start the experiment
                    try
                    {
                        // wait for the experiment to finish (ignore the OperationCanceledException if the experiment is canceled)
                        var result = await experimentWorker.DataUpdated
                            .Catch<CwEprData, OperationCanceledException>(ex => Observable.Empty<CwEprData>())
                            .LastAsync();
                        
                        // when the experiment finishes, set up the menu save button to display the save file dalog if it is clicked
                        // before the start button is clicked again
                        menuSave.GetClickObservable()
                            .TakeUntil(prepareStartButton.GetClickObservable())
                            .Subscribe(args => ShowSaveDialog(experiment, result));
                    }
                    catch (Exception ex)
                    {
                        // if an error occurs during the experiment, let the user know and close the form
                        MessageBox.Show(string.Format("Experiment failed due to error: {0}.", ex.Message));
                        Close();
                    }
                }

                // re-enable the UI controls
                SetUiFinished();
            }

        }

        // show the save file dialog for the experiment
        private void ShowSaveDialog(CwEprExperiment experiment, CwEprData experimentResult)
        {
            // show dialog to ask the user where to save files
            SaveFileDialog saveDialog = new SaveFileDialog();
            saveDialog.Filter = "Comma-separated value files (*.csv)|*.csv";
            saveDialog.RestoreDirectory = true;

            // if the user clicks ok
            if (saveDialog.ShowDialog() == DialogResult.OK)
            {
                // write the CSV rows in the experiment result to the specified file
                Stream dataFileStream;
                if ((dataFileStream = saveDialog.OpenFile()) != null)
                {
                    using (var writer = new StreamWriter(dataFileStream))
                    {
                        foreach (var line in experimentResult.CsvRows())
                        {
                            writer.WriteLine(line);
                        }
                    }
                    dataFileStream.Close();
                }

                // also write a txt file describing the experiment parameters in a JSON format
                Stream descriptionFileStream;
                if ((descriptionFileStream = File.Open(Path.ChangeExtension(saveDialog.FileName, "txt"), FileMode.Create)) != null)
                {
                    using (var writer = new StreamWriter(descriptionFileStream))
                    {
                        var json = JObject.FromObject(new
                            {
                                experimentParameters = experiment,
                                sampleNotes = sampleNotes.Text,
                                experimentNotes = experimentNotes.Text
                            });
                        writer.Write(json.ToString());
                    }
                }

            }
        }

        // when the form is closed, also close the connections to the hardware
        private async void MainWindow_FormClosed(object sender, FormClosedEventArgs e)
        {
            await magnetControllerSession.CloseSessionAsync().StartAsTask();
            await picoSession.CloseSessionAsync().StartAsTask();
        }
    }
}
