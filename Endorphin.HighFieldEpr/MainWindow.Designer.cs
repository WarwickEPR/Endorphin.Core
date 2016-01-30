// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

namespace Endorphin.TempApp
{
    partial class MainWindow
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.menuStrip = new System.Windows.Forms.MenuStrip();
            this.fileToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.menuSave = new System.Windows.Forms.ToolStripMenuItem();
            this.controls = new System.Windows.Forms.Panel();
            this.stopButton = new System.Windows.Forms.Button();
            this.experimentNotesGroup = new System.Windows.Forms.GroupBox();
            this.experimentNotes = new System.Windows.Forms.TextBox();
            this.stopToZero = new System.Windows.Forms.Button();
            this.stopAfterScan = new System.Windows.Forms.Button();
            this.prepareStartButton = new System.Windows.Forms.Button();
            this.sampleNotesGroup = new System.Windows.Forms.GroupBox();
            this.sampleNotes = new System.Windows.Forms.TextBox();
            this.acquisitionGroup = new System.Windows.Forms.GroupBox();
            this.estimatedTimeLabel = new System.Windows.Forms.Label();
            this.numberOfScansLabel = new System.Windows.Forms.Label();
            this.numberOfScans = new System.Windows.Forms.NumericUpDown();
            this.quadratureDetection = new System.Windows.Forms.CheckBox();
            this.downsamplingRatio = new System.Windows.Forms.NumericUpDown();
            this.downsamplingEnabled = new System.Windows.Forms.CheckBox();
            this.sampleIntervalLabel = new System.Windows.Forms.Label();
            this.sampleInterval = new System.Windows.Forms.NumericUpDown();
            this.magneticFieldGroup = new System.Windows.Forms.GroupBox();
            this.returnToZero = new System.Windows.Forms.CheckBox();
            this.currentRangeLabel = new System.Windows.Forms.Label();
            this.rampRate = new System.Windows.Forms.ComboBox();
            this.rampRateLabel = new System.Windows.Forms.Label();
            this.magnetControllerSteps = new System.Windows.Forms.NumericUpDown();
            this.magnetControllerStepsLabel = new System.Windows.Forms.Label();
            this.finalFieldLabel = new System.Windows.Forms.Label();
            this.finalField = new System.Windows.Forms.NumericUpDown();
            this.startingField = new System.Windows.Forms.NumericUpDown();
            this.staticFieldLabel = new System.Windows.Forms.Label();
            this.resultsGroup = new System.Windows.Forms.GroupBox();
            this.menuStrip.SuspendLayout();
            this.controls.SuspendLayout();
            this.experimentNotesGroup.SuspendLayout();
            this.sampleNotesGroup.SuspendLayout();
            this.acquisitionGroup.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.numberOfScans)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.downsamplingRatio)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.sampleInterval)).BeginInit();
            this.magneticFieldGroup.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.magnetControllerSteps)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.finalField)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.startingField)).BeginInit();
            this.SuspendLayout();
            // 
            // menuStrip
            // 
            this.menuStrip.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.fileToolStripMenuItem});
            this.menuStrip.Location = new System.Drawing.Point(0, 0);
            this.menuStrip.Name = "menuStrip";
            this.menuStrip.Padding = new System.Windows.Forms.Padding(12, 4, 0, 4);
            this.menuStrip.Size = new System.Drawing.Size(1996, 46);
            this.menuStrip.TabIndex = 0;
            this.menuStrip.Text = "menuStrip1";
            // 
            // fileToolStripMenuItem
            // 
            this.fileToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.menuSave});
            this.fileToolStripMenuItem.Name = "fileToolStripMenuItem";
            this.fileToolStripMenuItem.Size = new System.Drawing.Size(64, 38);
            this.fileToolStripMenuItem.Text = "File";
            // 
            // menuSave
            // 
            this.menuSave.Enabled = false;
            this.menuSave.Name = "menuSave";
            this.menuSave.Size = new System.Drawing.Size(244, 36);
            this.menuSave.Text = "Save";
            // 
            // controls
            // 
            this.controls.Controls.Add(this.stopButton);
            this.controls.Controls.Add(this.experimentNotesGroup);
            this.controls.Controls.Add(this.stopToZero);
            this.controls.Controls.Add(this.stopAfterScan);
            this.controls.Controls.Add(this.prepareStartButton);
            this.controls.Controls.Add(this.sampleNotesGroup);
            this.controls.Controls.Add(this.acquisitionGroup);
            this.controls.Controls.Add(this.magneticFieldGroup);
            this.controls.Location = new System.Drawing.Point(0, 52);
            this.controls.Margin = new System.Windows.Forms.Padding(6);
            this.controls.Name = "controls";
            this.controls.Size = new System.Drawing.Size(624, 1327);
            this.controls.TabIndex = 1;
            // 
            // stopButton
            // 
            this.stopButton.Enabled = false;
            this.stopButton.Location = new System.Drawing.Point(306, 1210);
            this.stopButton.Margin = new System.Windows.Forms.Padding(6);
            this.stopButton.Name = "stopButton";
            this.stopButton.Size = new System.Drawing.Size(302, 44);
            this.stopButton.TabIndex = 16;
            this.stopButton.Text = "Stop";
            this.stopButton.UseVisualStyleBackColor = true;
            // 
            // experimentNotesGroup
            // 
            this.experimentNotesGroup.Controls.Add(this.experimentNotes);
            this.experimentNotesGroup.Location = new System.Drawing.Point(24, 925);
            this.experimentNotesGroup.Margin = new System.Windows.Forms.Padding(6);
            this.experimentNotesGroup.Name = "experimentNotesGroup";
            this.experimentNotesGroup.Padding = new System.Windows.Forms.Padding(6);
            this.experimentNotesGroup.Size = new System.Drawing.Size(594, 256);
            this.experimentNotesGroup.TabIndex = 12;
            this.experimentNotesGroup.TabStop = false;
            this.experimentNotesGroup.Text = "Experiment notes";
            // 
            // experimentNotes
            // 
            this.experimentNotes.Location = new System.Drawing.Point(18, 37);
            this.experimentNotes.Margin = new System.Windows.Forms.Padding(6);
            this.experimentNotes.Multiline = true;
            this.experimentNotes.Name = "experimentNotes";
            this.experimentNotes.Size = new System.Drawing.Size(562, 204);
            this.experimentNotes.TabIndex = 11;
            // 
            // stopToZero
            // 
            this.stopToZero.Enabled = false;
            this.stopToZero.Location = new System.Drawing.Point(306, 1265);
            this.stopToZero.Margin = new System.Windows.Forms.Padding(6);
            this.stopToZero.Name = "stopToZero";
            this.stopToZero.Size = new System.Drawing.Size(302, 44);
            this.stopToZero.TabIndex = 15;
            this.stopToZero.Text = "Stop and return to zero";
            this.stopToZero.UseVisualStyleBackColor = true;
            // 
            // stopAfterScan
            // 
            this.stopAfterScan.Enabled = false;
            this.stopAfterScan.Location = new System.Drawing.Point(42, 1265);
            this.stopAfterScan.Margin = new System.Windows.Forms.Padding(6);
            this.stopAfterScan.Name = "stopAfterScan";
            this.stopAfterScan.Size = new System.Drawing.Size(252, 44);
            this.stopAfterScan.TabIndex = 14;
            this.stopAfterScan.Text = "Stop after scan";
            this.stopAfterScan.UseVisualStyleBackColor = true;
            // 
            // prepareStartButton
            // 
            this.prepareStartButton.Location = new System.Drawing.Point(42, 1210);
            this.prepareStartButton.Margin = new System.Windows.Forms.Padding(6);
            this.prepareStartButton.Name = "prepareStartButton";
            this.prepareStartButton.Size = new System.Drawing.Size(252, 44);
            this.prepareStartButton.TabIndex = 13;
            this.prepareStartButton.Text = "Prepare and start";
            this.prepareStartButton.UseVisualStyleBackColor = true;
            this.prepareStartButton.Click += new System.EventHandler(this.prepareStartButton_Click);
            // 
            // sampleNotesGroup
            // 
            this.sampleNotesGroup.Controls.Add(this.sampleNotes);
            this.sampleNotesGroup.Location = new System.Drawing.Point(24, 658);
            this.sampleNotesGroup.Margin = new System.Windows.Forms.Padding(6);
            this.sampleNotesGroup.Name = "sampleNotesGroup";
            this.sampleNotesGroup.Padding = new System.Windows.Forms.Padding(6);
            this.sampleNotesGroup.Size = new System.Drawing.Size(594, 256);
            this.sampleNotesGroup.TabIndex = 2;
            this.sampleNotesGroup.TabStop = false;
            this.sampleNotesGroup.Text = "Sample notes";
            // 
            // sampleNotes
            // 
            this.sampleNotes.Location = new System.Drawing.Point(18, 37);
            this.sampleNotes.Margin = new System.Windows.Forms.Padding(6);
            this.sampleNotes.Multiline = true;
            this.sampleNotes.Name = "sampleNotes";
            this.sampleNotes.Size = new System.Drawing.Size(562, 204);
            this.sampleNotes.TabIndex = 11;
            // 
            // acquisitionGroup
            // 
            this.acquisitionGroup.Controls.Add(this.estimatedTimeLabel);
            this.acquisitionGroup.Controls.Add(this.numberOfScansLabel);
            this.acquisitionGroup.Controls.Add(this.numberOfScans);
            this.acquisitionGroup.Controls.Add(this.quadratureDetection);
            this.acquisitionGroup.Controls.Add(this.downsamplingRatio);
            this.acquisitionGroup.Controls.Add(this.downsamplingEnabled);
            this.acquisitionGroup.Controls.Add(this.sampleIntervalLabel);
            this.acquisitionGroup.Controls.Add(this.sampleInterval);
            this.acquisitionGroup.Location = new System.Drawing.Point(24, 360);
            this.acquisitionGroup.Margin = new System.Windows.Forms.Padding(6);
            this.acquisitionGroup.Name = "acquisitionGroup";
            this.acquisitionGroup.Padding = new System.Windows.Forms.Padding(6);
            this.acquisitionGroup.Size = new System.Drawing.Size(594, 287);
            this.acquisitionGroup.TabIndex = 1;
            this.acquisitionGroup.TabStop = false;
            this.acquisitionGroup.Text = "Acquisition";
            // 
            // estimatedTimeLabel
            // 
            this.estimatedTimeLabel.AutoSize = true;
            this.estimatedTimeLabel.Location = new System.Drawing.Point(12, 246);
            this.estimatedTimeLabel.Margin = new System.Windows.Forms.Padding(6, 0, 6, 0);
            this.estimatedTimeLabel.Name = "estimatedTimeLabel";
            this.estimatedTimeLabel.Size = new System.Drawing.Size(238, 25);
            this.estimatedTimeLabel.TabIndex = 7;
            this.estimatedTimeLabel.Text = "Estimated duration: 60s";
            // 
            // numberOfScansLabel
            // 
            this.numberOfScansLabel.AutoSize = true;
            this.numberOfScansLabel.Location = new System.Drawing.Point(12, 42);
            this.numberOfScansLabel.Margin = new System.Windows.Forms.Padding(6, 0, 6, 0);
            this.numberOfScansLabel.Name = "numberOfScansLabel";
            this.numberOfScansLabel.Size = new System.Drawing.Size(174, 25);
            this.numberOfScansLabel.TabIndex = 6;
            this.numberOfScansLabel.Text = "Number of scans";
            // 
            // numberOfScans
            // 
            this.numberOfScans.Location = new System.Drawing.Point(412, 38);
            this.numberOfScans.Margin = new System.Windows.Forms.Padding(6);
            this.numberOfScans.Maximum = new decimal(new int[] {
            1024,
            0,
            0,
            0});
            this.numberOfScans.Minimum = new decimal(new int[] {
            1,
            0,
            0,
            0});
            this.numberOfScans.Name = "numberOfScans";
            this.numberOfScans.Size = new System.Drawing.Size(168, 31);
            this.numberOfScans.TabIndex = 6;
            this.numberOfScans.Value = new decimal(new int[] {
            1,
            0,
            0,
            0});
            // 
            // quadratureDetection
            // 
            this.quadratureDetection.AutoSize = true;
            this.quadratureDetection.Checked = true;
            this.quadratureDetection.CheckState = System.Windows.Forms.CheckState.Checked;
            this.quadratureDetection.Location = new System.Drawing.Point(18, 185);
            this.quadratureDetection.Margin = new System.Windows.Forms.Padding(6);
            this.quadratureDetection.Name = "quadratureDetection";
            this.quadratureDetection.Size = new System.Drawing.Size(246, 29);
            this.quadratureDetection.TabIndex = 10;
            this.quadratureDetection.Text = "Quadrature detection";
            this.quadratureDetection.UseVisualStyleBackColor = true;
            // 
            // downsamplingRatio
            // 
            this.downsamplingRatio.Location = new System.Drawing.Point(412, 138);
            this.downsamplingRatio.Margin = new System.Windows.Forms.Padding(6);
            this.downsamplingRatio.Maximum = new decimal(new int[] {
            100000,
            0,
            0,
            0});
            this.downsamplingRatio.Minimum = new decimal(new int[] {
            1,
            0,
            0,
            0});
            this.downsamplingRatio.Name = "downsamplingRatio";
            this.downsamplingRatio.Size = new System.Drawing.Size(170, 31);
            this.downsamplingRatio.TabIndex = 9;
            this.downsamplingRatio.Value = new decimal(new int[] {
            2000,
            0,
            0,
            0});
            // 
            // downsamplingEnabled
            // 
            this.downsamplingEnabled.AutoSize = true;
            this.downsamplingEnabled.Checked = true;
            this.downsamplingEnabled.CheckState = System.Windows.Forms.CheckState.Checked;
            this.downsamplingEnabled.Location = new System.Drawing.Point(18, 140);
            this.downsamplingEnabled.Margin = new System.Windows.Forms.Padding(6);
            this.downsamplingEnabled.Name = "downsamplingEnabled";
            this.downsamplingEnabled.Size = new System.Drawing.Size(232, 29);
            this.downsamplingEnabled.TabIndex = 8;
            this.downsamplingEnabled.Text = "Downsampling ratio";
            this.downsamplingEnabled.UseVisualStyleBackColor = true;
            this.downsamplingEnabled.CheckedChanged += new System.EventHandler(this.downsamplingEnabled_CheckedChanged);
            // 
            // sampleIntervalLabel
            // 
            this.sampleIntervalLabel.AutoSize = true;
            this.sampleIntervalLabel.Location = new System.Drawing.Point(12, 92);
            this.sampleIntervalLabel.Margin = new System.Windows.Forms.Padding(6, 0, 6, 0);
            this.sampleIntervalLabel.Name = "sampleIntervalLabel";
            this.sampleIntervalLabel.Size = new System.Drawing.Size(203, 25);
            this.sampleIntervalLabel.TabIndex = 1;
            this.sampleIntervalLabel.Text = "Sample interval (us)";
            // 
            // sampleInterval
            // 
            this.sampleInterval.Location = new System.Drawing.Point(412, 88);
            this.sampleInterval.Margin = new System.Windows.Forms.Padding(6);
            this.sampleInterval.Maximum = new decimal(new int[] {
            1000000,
            0,
            0,
            0});
            this.sampleInterval.Minimum = new decimal(new int[] {
            10,
            0,
            0,
            0});
            this.sampleInterval.Name = "sampleInterval";
            this.sampleInterval.Size = new System.Drawing.Size(170, 31);
            this.sampleInterval.TabIndex = 7;
            this.sampleInterval.Tag = "";
            this.sampleInterval.Value = new decimal(new int[] {
            100,
            0,
            0,
            0});
            // 
            // magneticFieldGroup
            // 
            this.magneticFieldGroup.Controls.Add(this.returnToZero);
            this.magneticFieldGroup.Controls.Add(this.currentRangeLabel);
            this.magneticFieldGroup.Controls.Add(this.rampRate);
            this.magneticFieldGroup.Controls.Add(this.rampRateLabel);
            this.magneticFieldGroup.Controls.Add(this.magnetControllerSteps);
            this.magneticFieldGroup.Controls.Add(this.magnetControllerStepsLabel);
            this.magneticFieldGroup.Controls.Add(this.finalFieldLabel);
            this.magneticFieldGroup.Controls.Add(this.finalField);
            this.magneticFieldGroup.Controls.Add(this.startingField);
            this.magneticFieldGroup.Controls.Add(this.staticFieldLabel);
            this.magneticFieldGroup.Location = new System.Drawing.Point(24, 6);
            this.magneticFieldGroup.Margin = new System.Windows.Forms.Padding(6);
            this.magneticFieldGroup.Name = "magneticFieldGroup";
            this.magneticFieldGroup.Padding = new System.Windows.Forms.Padding(6);
            this.magneticFieldGroup.Size = new System.Drawing.Size(594, 342);
            this.magneticFieldGroup.TabIndex = 0;
            this.magneticFieldGroup.TabStop = false;
            this.magneticFieldGroup.Text = "Magnetic field";
            // 
            // returnToZero
            // 
            this.returnToZero.AutoSize = true;
            this.returnToZero.Location = new System.Drawing.Point(324, 240);
            this.returnToZero.Margin = new System.Windows.Forms.Padding(6);
            this.returnToZero.Name = "returnToZero";
            this.returnToZero.Size = new System.Drawing.Size(253, 29);
            this.returnToZero.TabIndex = 5;
            this.returnToZero.Text = "Return to zero current";
            this.returnToZero.UseVisualStyleBackColor = true;
            // 
            // currentRangeLabel
            // 
            this.currentRangeLabel.AutoSize = true;
            this.currentRangeLabel.Location = new System.Drawing.Point(12, 302);
            this.currentRangeLabel.Margin = new System.Windows.Forms.Padding(6, 0, 6, 0);
            this.currentRangeLabel.Name = "currentRangeLabel";
            this.currentRangeLabel.Size = new System.Drawing.Size(515, 25);
            this.currentRangeLabel.TabIndex = 9;
            this.currentRangeLabel.Text = "Magnet controller current range: 0.0000A to 1.0000A";
            // 
            // rampRate
            // 
            this.rampRate.FormattingEnabled = true;
            this.rampRate.Location = new System.Drawing.Point(412, 188);
            this.rampRate.Margin = new System.Windows.Forms.Padding(6);
            this.rampRate.Name = "rampRate";
            this.rampRate.Size = new System.Drawing.Size(168, 33);
            this.rampRate.TabIndex = 4;
            this.rampRate.SelectedIndexChanged += new System.EventHandler(this.rampRate_SelectedIndexChanged);
            // 
            // rampRateLabel
            // 
            this.rampRateLabel.AutoSize = true;
            this.rampRateLabel.Location = new System.Drawing.Point(12, 194);
            this.rampRateLabel.Margin = new System.Windows.Forms.Padding(6, 0, 6, 0);
            this.rampRateLabel.Name = "rampRateLabel";
            this.rampRateLabel.Size = new System.Drawing.Size(162, 25);
            this.rampRateLabel.TabIndex = 7;
            this.rampRateLabel.Text = "Ramp rate (A/s)";
            // 
            // magnetControllerSteps
            // 
            this.magnetControllerSteps.Location = new System.Drawing.Point(412, 138);
            this.magnetControllerSteps.Margin = new System.Windows.Forms.Padding(6);
            this.magnetControllerSteps.Name = "magnetControllerSteps";
            this.magnetControllerSteps.Size = new System.Drawing.Size(170, 31);
            this.magnetControllerSteps.TabIndex = 3;
            // 
            // magnetControllerStepsLabel
            // 
            this.magnetControllerStepsLabel.AutoSize = true;
            this.magnetControllerStepsLabel.Location = new System.Drawing.Point(12, 142);
            this.magnetControllerStepsLabel.Margin = new System.Windows.Forms.Padding(6, 0, 6, 0);
            this.magnetControllerStepsLabel.Name = "magnetControllerStepsLabel";
            this.magnetControllerStepsLabel.Size = new System.Drawing.Size(237, 25);
            this.magnetControllerStepsLabel.TabIndex = 4;
            this.magnetControllerStepsLabel.Text = "Magnet controller steps";
            // 
            // finalFieldLabel
            // 
            this.finalFieldLabel.AutoSize = true;
            this.finalFieldLabel.Location = new System.Drawing.Point(12, 90);
            this.finalFieldLabel.Margin = new System.Windows.Forms.Padding(6, 0, 6, 0);
            this.finalFieldLabel.Name = "finalFieldLabel";
            this.finalFieldLabel.Size = new System.Drawing.Size(138, 25);
            this.finalFieldLabel.TabIndex = 3;
            this.finalFieldLabel.Text = "Final field (T)";
            // 
            // finalField
            // 
            this.finalField.DecimalPlaces = 8;
            this.finalField.Location = new System.Drawing.Point(412, 87);
            this.finalField.Margin = new System.Windows.Forms.Padding(6);
            this.finalField.Name = "finalField";
            this.finalField.Size = new System.Drawing.Size(170, 31);
            this.finalField.TabIndex = 2;
            // 
            // startingField
            // 
            this.startingField.DecimalPlaces = 8;
            this.startingField.Location = new System.Drawing.Point(412, 37);
            this.startingField.Margin = new System.Windows.Forms.Padding(6);
            this.startingField.Name = "startingField";
            this.startingField.Size = new System.Drawing.Size(170, 31);
            this.startingField.TabIndex = 1;
            // 
            // staticFieldLabel
            // 
            this.staticFieldLabel.AutoSize = true;
            this.staticFieldLabel.Location = new System.Drawing.Point(12, 40);
            this.staticFieldLabel.Margin = new System.Windows.Forms.Padding(6, 0, 6, 0);
            this.staticFieldLabel.Name = "staticFieldLabel";
            this.staticFieldLabel.Size = new System.Drawing.Size(165, 25);
            this.staticFieldLabel.TabIndex = 0;
            this.staticFieldLabel.Text = "Starting field (T)";
            // 
            // resultsGroup
            // 
            this.resultsGroup.Location = new System.Drawing.Point(636, 58);
            this.resultsGroup.Margin = new System.Windows.Forms.Padding(6);
            this.resultsGroup.Name = "resultsGroup";
            this.resultsGroup.Padding = new System.Windows.Forms.Padding(6);
            this.resultsGroup.Size = new System.Drawing.Size(1356, 1321);
            this.resultsGroup.TabIndex = 2;
            this.resultsGroup.TabStop = false;
            this.resultsGroup.Text = "CW EPR experiment (waiting)";
            // 
            // MainWindow
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(12F, 25F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(1996, 1340);
            this.Controls.Add(this.resultsGroup);
            this.Controls.Add(this.controls);
            this.Controls.Add(this.menuStrip);
            this.MainMenuStrip = this.menuStrip;
            this.Margin = new System.Windows.Forms.Padding(6);
            this.MaximumSize = new System.Drawing.Size(2022, 1411);
            this.MinimumSize = new System.Drawing.Size(2022, 1411);
            this.Name = "MainWindow";
            this.Text = "Endorphin CW EPR";
            this.FormClosed += new System.Windows.Forms.FormClosedEventHandler(this.MainWindow_FormClosed);
            this.Load += new System.EventHandler(this.MainWindow_Load);
            this.menuStrip.ResumeLayout(false);
            this.menuStrip.PerformLayout();
            this.controls.ResumeLayout(false);
            this.experimentNotesGroup.ResumeLayout(false);
            this.experimentNotesGroup.PerformLayout();
            this.sampleNotesGroup.ResumeLayout(false);
            this.sampleNotesGroup.PerformLayout();
            this.acquisitionGroup.ResumeLayout(false);
            this.acquisitionGroup.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.numberOfScans)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.downsamplingRatio)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.sampleInterval)).EndInit();
            this.magneticFieldGroup.ResumeLayout(false);
            this.magneticFieldGroup.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.magnetControllerSteps)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.finalField)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.startingField)).EndInit();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.MenuStrip menuStrip;
        private System.Windows.Forms.ToolStripMenuItem fileToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem menuSave;
        private System.Windows.Forms.Panel controls;
        private System.Windows.Forms.GroupBox magneticFieldGroup;
        private System.Windows.Forms.NumericUpDown magnetControllerSteps;
        private System.Windows.Forms.Label magnetControllerStepsLabel;
        private System.Windows.Forms.Label finalFieldLabel;
        private System.Windows.Forms.NumericUpDown finalField;
        private System.Windows.Forms.NumericUpDown startingField;
        private System.Windows.Forms.Label staticFieldLabel;
        private System.Windows.Forms.Label rampRateLabel;
        private System.Windows.Forms.Label currentRangeLabel;
        private System.Windows.Forms.ComboBox rampRate;
        private System.Windows.Forms.GroupBox acquisitionGroup;
        private System.Windows.Forms.Label sampleIntervalLabel;
        private System.Windows.Forms.NumericUpDown sampleInterval;
        private System.Windows.Forms.CheckBox returnToZero;
        private System.Windows.Forms.NumericUpDown downsamplingRatio;
        private System.Windows.Forms.CheckBox downsamplingEnabled;
        private System.Windows.Forms.CheckBox quadratureDetection;
        private System.Windows.Forms.Label estimatedTimeLabel;
        private System.Windows.Forms.Label numberOfScansLabel;
        private System.Windows.Forms.NumericUpDown numberOfScans;
        private System.Windows.Forms.Button stopToZero;
        private System.Windows.Forms.Button stopAfterScan;
        private System.Windows.Forms.Button prepareStartButton;
        private System.Windows.Forms.GroupBox sampleNotesGroup;
        private System.Windows.Forms.TextBox sampleNotes;
        private System.Windows.Forms.GroupBox resultsGroup;
        private System.Windows.Forms.Button stopButton;
        private System.Windows.Forms.GroupBox experimentNotesGroup;
        private System.Windows.Forms.TextBox experimentNotes;
    }
}

