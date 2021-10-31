using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using System.Drawing.Drawing2D;
using System;
using System.Collections.Generic;
using csharp;
using System.Data;

namespace csharp
{
    class MainForm :Form
    {
        private Button btnShowData;
        private Button btnInsertItem;
        private IList<Item> Items;
        private GildedRose app;
        public MainForm()
        {
            InitializeComponent();
            Items = new List<Item>{
            new Item {Name = "+5 Dexterity Vest", SellIn = 5, Quality = 20},
            new Item {Name = "Aged Brie", SellIn = 8, Quality = 0},
            new Item {Name = "Elixir of the Mongoose", SellIn = 10, Quality = 7},
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

             app = new GildedRose();
        }

        private void InitializeComponent()
        {
            this.btnShowData = new System.Windows.Forms.Button();
            this.btnInsertItem = new System.Windows.Forms.Button();
            this.SuspendLayout();
            // 
            // btnShowData
            // 
            this.btnShowData.Location = new System.Drawing.Point(61, 53);
            this.btnShowData.Name = "btnShowData";
            this.btnShowData.Size = new System.Drawing.Size(163, 37);
            this.btnShowData.TabIndex = 0;
            this.btnShowData.Text = "Show Items";
            this.btnShowData.UseVisualStyleBackColor = true;
            this.btnShowData.Click += new System.EventHandler(this.btnShowData_Click);
            // 
            // btnInsertItem
            // 
            this.btnInsertItem.Location = new System.Drawing.Point(61, 136);
            this.btnInsertItem.Name = "btnInsertItem";
            this.btnInsertItem.Size = new System.Drawing.Size(163, 41);
            this.btnInsertItem.TabIndex = 1;
            this.btnInsertItem.Text = "Insert item";
            this.btnInsertItem.UseVisualStyleBackColor = true;
            this.btnInsertItem.Click += new System.EventHandler(this.btnInsertItem_Click);
            // 
            // MainForm
            // 
            this.ClientSize = new System.Drawing.Size(282, 253);
            this.Controls.Add(this.btnInsertItem);
            this.Controls.Add(this.btnShowData);
            this.Name = "MainForm";
            this.Text = "Gilded Rose";
            this.Load += new System.EventHandler(this.MainForm_Load);
            this.ResumeLayout(false);

        }

        private void MainForm_Load(object sender, EventArgs e)
        {

        }
        public static void Main(string[] args)
        {
            var mainForm = new MainForm();
            mainForm.ShowDialog();
        }

        private void btnShowData_Click(object sender, EventArgs e)
        {
 
            var showDataForm = new showDataForm(app, Items);
            showDataForm.ShowDialog();
           
        }

        private void btnInsertItem_Click(object sender, EventArgs e)
        {

            var Additemform = new addItemForm(Items);
            Additemform.ShowDialog();

        }
    }
}
