using System.Windows.Forms;
using System.Drawing.Drawing2D;
using System;
using System.Collections.Generic;
using csharp;
using System.Data;

public partial class showDataForm : Form
{
    private DataGridView dgvData;
    private Label lblDay;
    private DataGridViewTextBoxColumn itemName;
    private DataGridViewTextBoxColumn SellIn;
    private DataGridViewTextBoxColumn Quality;
    private GildedRose app;
    private ComboBox cbDay;
    private IList<Item> Items;

    public showDataForm(GildedRose app, IList<Item> Items)
    {
        this.app = app;
        this.Items = Items;
        InitializeComponent();
        for(int i=1; i<31; i++)
        {
            cbDay.Items.Add(i);
        }
    }

    private void InitializeComponent()
    {
            this.dgvData = new System.Windows.Forms.DataGridView();
            this.itemName = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.SellIn = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.Quality = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.lblDay = new System.Windows.Forms.Label();
            this.cbDay = new System.Windows.Forms.ComboBox();
            ((System.ComponentModel.ISupportInitialize)(this.dgvData)).BeginInit();
            this.SuspendLayout();
            // 
            // dgvData
            // 
            this.dgvData.AllowUserToOrderColumns = true;
            this.dgvData.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize;
            this.dgvData.Columns.AddRange(new System.Windows.Forms.DataGridViewColumn[] {
            this.itemName,
            this.SellIn,
            this.Quality});
            this.dgvData.Location = new System.Drawing.Point(79, 128);
            this.dgvData.Name = "dgvData";
            this.dgvData.RowHeadersWidth = 51;
            this.dgvData.RowTemplate.Height = 24;
            this.dgvData.Size = new System.Drawing.Size(426, 327);
            this.dgvData.TabIndex = 0;
            this.dgvData.CellContentClick += new System.Windows.Forms.DataGridViewCellEventHandler(this.dataGridView1_CellContentClick_1);
            // 
            // itemName
            // 
            this.itemName.HeaderText = "itemName";
            this.itemName.MinimumWidth = 6;
            this.itemName.Name = "itemName";
            this.itemName.ReadOnly = true;
            this.itemName.Width = 125;
            // 
            // SellIn
            // 
            this.SellIn.HeaderText = "SellIn";
            this.SellIn.MinimumWidth = 6;
            this.SellIn.Name = "SellIn";
            this.SellIn.ReadOnly = true;
            this.SellIn.Width = 125;
            // 
            // Quality
            // 
            this.Quality.HeaderText = "Quality";
            this.Quality.MinimumWidth = 6;
            this.Quality.Name = "Quality";
            this.Quality.ReadOnly = true;
            this.Quality.Width = 125;
            // 
            // lblDay
            // 
            this.lblDay.AutoSize = true;
            this.lblDay.Location = new System.Drawing.Point(171, 48);
            this.lblDay.Name = "lblDay";
            this.lblDay.Size = new System.Drawing.Size(33, 17);
            this.lblDay.TabIndex = 3;
            this.lblDay.Text = "Day";
            // 
            // cbDay
            // 
            this.cbDay.FormattingEnabled = true;
            this.cbDay.Location = new System.Drawing.Point(229, 45);
            this.cbDay.Name = "cbDay";
            this.cbDay.Size = new System.Drawing.Size(121, 24);
            this.cbDay.TabIndex = 4;
            this.cbDay.SelectedIndexChanged += new System.EventHandler(this.cbDay_SelectedIndexChanged);
            // 
            // showDataForm
            // 
            this.ClientSize = new System.Drawing.Size(554, 460);
            this.Controls.Add(this.cbDay);
            this.Controls.Add(this.lblDay);
            this.Controls.Add(this.dgvData);
            this.Name = "showDataForm";
            this.Text = "Show Data";
            this.Load += new System.EventHandler(this.showDataForm_Load);
            ((System.ComponentModel.ISupportInitialize)(this.dgvData)).EndInit();
            this.ResumeLayout(false);
            this.PerformLayout();

    }

 

    private void dataGridView1_CellContentClick(object sender, DataGridViewCellEventArgs e)
    {

    }

    private void dataGridView1_CellContentClick_1(object sender, DataGridViewCellEventArgs e)
    {

    }

    private void btnShowData_Click(object sender, EventArgs e)
    {
        
    }

    private void showDataForm_Load(object sender, EventArgs e)
    {

    }

    private void cbDay_SelectedIndexChanged(object sender, EventArgs e)
    {
        dgvData.Rows.Clear();
        int counter = 0;
        IList<Item> tempItems = new List<Item>();
        foreach (var it in Items)
        {
            tempItems.Add(new Item(it));
        }

        for (var i = 0; i < (int)cbDay.SelectedItem; i++)
        {
            app.UpdateQuality(tempItems);
        }

        for (var j = 0; j < tempItems.Count; j++)
        {
            if (tempItems[j].Quality > 0)
            {
                DataGridViewRow row = (DataGridViewRow)dgvData.Rows[counter].Clone();
                row.Cells[0].Value = tempItems[j].Name;
                row.Cells[1].Value = tempItems[j].SellIn < 0 ? 0 : tempItems[j].SellIn;
                row.Cells[2].Value = tempItems[j].Quality;
                dgvData.Rows.Add(row);
                counter++;
            }
        }
    }
}