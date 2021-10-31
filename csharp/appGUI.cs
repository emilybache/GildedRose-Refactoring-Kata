using System.Windows.Forms;
using System.Drawing.Drawing2D;
using System;
using System.Collections.Generic;
using csharp;
using System.Data;

public partial class appGUI : Form
{
    private DataGridView dataGridView1;
    private DataGridViewTextBoxColumn Day;
    private DataGridViewTextBoxColumn itemName;
    private DataGridViewTextBoxColumn SellIn;
    private DataGridViewTextBoxColumn Quality;
    private Button button1;

    public appGUI()
    {
        InitializeComponent();
    }

    private void InitializeComponent()
    {
            this.dataGridView1 = new System.Windows.Forms.DataGridView();
            this.button1 = new System.Windows.Forms.Button();
            this.Day = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.itemName = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.SellIn = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.Quality = new System.Windows.Forms.DataGridViewTextBoxColumn();
            ((System.ComponentModel.ISupportInitialize)(this.dataGridView1)).BeginInit();
            this.SuspendLayout();
            // 
            // dataGridView1
            // 
            this.dataGridView1.AllowUserToOrderColumns = true;
            this.dataGridView1.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize;
            this.dataGridView1.Columns.AddRange(new System.Windows.Forms.DataGridViewColumn[] {
            this.Day,
            this.itemName,
            this.SellIn,
            this.Quality});
            this.dataGridView1.Location = new System.Drawing.Point(-7, 56);
            this.dataGridView1.Name = "dataGridView1";
            this.dataGridView1.RowHeadersWidth = 51;
            this.dataGridView1.RowTemplate.Height = 24;
            this.dataGridView1.Size = new System.Drawing.Size(560, 414);
            this.dataGridView1.TabIndex = 0;
            this.dataGridView1.CellContentClick += new System.Windows.Forms.DataGridViewCellEventHandler(this.dataGridView1_CellContentClick_1);
            // 
            // button1
            // 
            this.button1.Location = new System.Drawing.Point(199, 12);
            this.button1.Name = "button1";
            this.button1.Size = new System.Drawing.Size(181, 23);
            this.button1.TabIndex = 1;
            this.button1.Text = "Show Data";
            this.button1.UseVisualStyleBackColor = true;
            this.button1.Click += new System.EventHandler(this.button1_Click);
            // 
            // Day
            // 
            this.Day.HeaderText = "Day";
            this.Day.MinimumWidth = 6;
            this.Day.Name = "Day";
            this.Day.ReadOnly = true;
            this.Day.Width = 125;
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
            // appGUI
            // 
            this.ClientSize = new System.Drawing.Size(554, 460);
            this.Controls.Add(this.button1);
            this.Controls.Add(this.dataGridView1);
            this.Name = "appGUI";
            this.Text = "appGUI";
            ((System.ComponentModel.ISupportInitialize)(this.dataGridView1)).EndInit();
            this.ResumeLayout(false);

    }

    public static void Main(string[] args)
    {
    var guiForm = new appGUI();
    guiForm.ShowDialog();
    }

    private void dataGridView1_CellContentClick(object sender, DataGridViewCellEventArgs e)
    {

    }

    private void dataGridView1_CellContentClick_1(object sender, DataGridViewCellEventArgs e)
    {

    }

    private void button1_Click(object sender, EventArgs e)
    {
        List<Item> Items = new List<Item>{
            new Item {Name = "+5 Dexterity Vest", SellIn = 10, Quality = 20},
            new Item {Name = "Aged Brie", SellIn = 2, Quality = 0},
            new Item {Name = "Elixir of the Mongoose", SellIn = 5, Quality = 7},
            new Item {Name = "Sulfuras, Hand of Ragnaros", SellIn = 0, Quality = 80},
            new Item {Name = "Sulfuras, Hand of Ragnaros", SellIn = -1, Quality = 80},
            new Item
            {
                Name = "Backstage passes to a TAFKAL80ETC concert",
                SellIn = 15,
                Quality = 20
            },
            new Item
            {
                Name = "Backstage passes to a TAFKAL80ETC concert",
                SellIn = 10,
                Quality = 49
            },
            new Item
            {
                Name = "Backstage passes to a TAFKAL80ETC concert",
                SellIn = 5,
                Quality = 49
            },
			// this conjured item does not work properly yet
			new Item {Name = "Conjured Mana Cake", SellIn = 3, Quality = 6}
        };

        var app = new GildedRose(Items);


        for (var i = 0; i < 31; i++)
        {
            Console.WriteLine("-------- day " + i + " --------");
            Console.WriteLine("name, sellIn, quality");
            for (var j = 0; j < Items.Count; j++)
            {
                DataGridViewRow row = (DataGridViewRow)dataGridView1.Rows[j].Clone();
                row.Cells[0].Value = i;
                row.Cells[1].Value = Items[j].Name;
                row.Cells[2].Value = Items[j].SellIn;
                row.Cells[3].Value = Items[j].Quality;
                dataGridView1.Rows.Add(row);
            }
            app.UpdateQuality();
        }
    }
}