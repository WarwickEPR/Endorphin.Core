namespace StreamingScope
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
            this.chartPanel = new System.Windows.Forms.Panel();
            this.buttonPanel = new System.Windows.Forms.Panel();
            this.stopButton = new System.Windows.Forms.Button();
            this.startButton = new System.Windows.Forms.Button();
            this.buttonPanel.SuspendLayout();
            this.SuspendLayout();
            // 
            // chartPanel
            // 
            this.chartPanel.Location = new System.Drawing.Point(24, 23);
            this.chartPanel.Margin = new System.Windows.Forms.Padding(6);
            this.chartPanel.Name = "chartPanel";
            this.chartPanel.Size = new System.Drawing.Size(1968, 1288);
            this.chartPanel.TabIndex = 0;
            // 
            // buttonPanel
            // 
            this.buttonPanel.Controls.Add(this.stopButton);
            this.buttonPanel.Controls.Add(this.startButton);
            this.buttonPanel.Location = new System.Drawing.Point(24, 1323);
            this.buttonPanel.Margin = new System.Windows.Forms.Padding(6);
            this.buttonPanel.Name = "buttonPanel";
            this.buttonPanel.Size = new System.Drawing.Size(1968, 56);
            this.buttonPanel.TabIndex = 1;
            // 
            // stopButton
            // 
            this.stopButton.Location = new System.Drawing.Point(1650, 6);
            this.stopButton.Margin = new System.Windows.Forms.Padding(6);
            this.stopButton.Name = "stopButton";
            this.stopButton.Size = new System.Drawing.Size(150, 44);
            this.stopButton.TabIndex = 1;
            this.stopButton.Text = "Stop";
            this.stopButton.UseVisualStyleBackColor = true;
            // 
            // startButton
            // 
            this.startButton.Location = new System.Drawing.Point(1812, 6);
            this.startButton.Margin = new System.Windows.Forms.Padding(6);
            this.startButton.Name = "startButton";
            this.startButton.Size = new System.Drawing.Size(150, 44);
            this.startButton.TabIndex = 0;
            this.startButton.Text = "Start";
            this.startButton.UseVisualStyleBackColor = true;
            this.startButton.Click += new System.EventHandler(this.startButton_Click);
            // 
            // MainWindow
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(12F, 25F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(2016, 1402);
            this.Controls.Add(this.buttonPanel);
            this.Controls.Add(this.chartPanel);
            this.Margin = new System.Windows.Forms.Padding(6);
            this.Name = "MainWindow";
            this.Text = "Streaming scope";
            this.FormClosing += new System.Windows.Forms.FormClosingEventHandler(this.MainWindow_FormClosing);
            this.buttonPanel.ResumeLayout(false);
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.Panel chartPanel;
        private System.Windows.Forms.Panel buttonPanel;
        private System.Windows.Forms.Button stopButton;
        private System.Windows.Forms.Button startButton;
    }
}

