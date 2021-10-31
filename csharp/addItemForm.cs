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
    class addItemForm :Form
    {
        private ComboBox cbType;
        private Label lblItemType;
        private Label lblSellIn;
        private Label lblQuality;
        private NumericUpDown nudlSellIn;
        private Button btnAdd;
        private NumericUpDown nudlQuality;
        private IList<Item> Items;
        public addItemForm(IList<Item> Items)
        {
            this.Items = Items;
            InitializeComponent();
        }

        private void InitializeComponent()
        {
            this.cbType = new System.Windows.Forms.ComboBox();
            this.lblItemType = new System.Windows.Forms.Label();
            this.lblSellIn = new System.Windows.Forms.Label();
            this.lblQuality = new System.Windows.Forms.Label();
            this.nudlSellIn = new System.Windows.Forms.NumericUpDown();
            this.nudlQuality = new System.Windows.Forms.NumericUpDown();
            this.btnAdd = new System.Windows.Forms.Button();
            ((System.ComponentModel.ISupportInitialize)(this.nudlSellIn)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.nudlQuality)).BeginInit();
            this.SuspendLayout();
            // 
            // cbType
            // 
            this.cbType.FormattingEnabled = true;
            this.cbType.Location = new System.Drawing.Point(99, 40);
            this.cbType.Name = "cbType";
            this.cbType.Size = new System.Drawing.Size(121, 24);
            this.cbType.TabIndex = 0;
            this.cbType.SelectedIndexChanged += new System.EventHandler(this.cbType_SelectedIndexChanged);
            // 
            // lblItemType
            // 
            this.lblItemType.AutoSize = true;
            this.lblItemType.Location = new System.Drawing.Point(13, 40);
            this.lblItemType.Name = "lblItemType";
            this.lblItemType.Size = new System.Drawing.Size(70, 17);
            this.lblItemType.TabIndex = 1;
            this.lblItemType.Text = "Item Type";
            // 
            // lblSellIn
            // 
            this.lblSellIn.AutoSize = true;
            this.lblSellIn.Location = new System.Drawing.Point(12, 86);
            this.lblSellIn.Name = "lblSellIn";
            this.lblSellIn.Size = new System.Drawing.Size(46, 17);
            this.lblSellIn.TabIndex = 2;
            this.lblSellIn.Text = "Sell In";
            // 
            // lblQuality
            // 
            this.lblQuality.AutoSize = true;
            this.lblQuality.Location = new System.Drawing.Point(13, 142);
            this.lblQuality.Name = "lblQuality";
            this.lblQuality.Size = new System.Drawing.Size(52, 17);
            this.lblQuality.TabIndex = 3;
            this.lblQuality.Text = "Quality";
            // 
            // nudlSellIn
            // 
            this.nudlSellIn.Location = new System.Drawing.Point(99, 86);
            this.nudlSellIn.Name = "nudlSellIn";
            this.nudlSellIn.Size = new System.Drawing.Size(120, 22);
            this.nudlSellIn.TabIndex = 4;
            // 
            // nudlQuality
            // 
            this.nudlQuality.Location = new System.Drawing.Point(99, 137);
            this.nudlQuality.Name = "nudlQuality";
            this.nudlQuality.Size = new System.Drawing.Size(120, 22);
            this.nudlQuality.TabIndex = 5;
            // 
            // btnAdd
            // 
            this.btnAdd.Location = new System.Drawing.Point(99, 202);
            this.btnAdd.Name = "btnAdd";
            this.btnAdd.Size = new System.Drawing.Size(75, 23);
            this.btnAdd.TabIndex = 6;
            this.btnAdd.Text = "Add Item";
            this.btnAdd.UseVisualStyleBackColor = true;
            this.btnAdd.Click += new System.EventHandler(this.btnAdd_Click);
            // 
            // addItemForm
            // 
            this.ClientSize = new System.Drawing.Size(282, 253);
            this.Controls.Add(this.btnAdd);
            this.Controls.Add(this.nudlQuality);
            this.Controls.Add(this.nudlSellIn);
            this.Controls.Add(this.lblQuality);
            this.Controls.Add(this.lblSellIn);
            this.Controls.Add(this.lblItemType);
            this.Controls.Add(this.cbType);
            this.Name = "addItemForm";
            this.Text = "Add an item";
            this.Load += new System.EventHandler(this.addItemForm_Load);
            ((System.ComponentModel.ISupportInitialize)(this.nudlSellIn)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.nudlQuality)).EndInit();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        private void btnAdd_Click(object sender, EventArgs e)
        {
            var itemType = cbType.SelectedItem;
            if(itemType == null)
            {
                MessageBox.Show("Please Select Item Type");
                return;
            }
            var sellIn = nudlSellIn.Value;
            var Quality = nudlQuality.Value;
            this.Items.Add(new Item { Name = itemType.ToString(), SellIn = (int)sellIn, Quality = (int)Quality });
            MessageBox.Show("Item Added Sucessfully");
            this.Hide();
        }

        private void addItemForm_Load(object sender, EventArgs e)
        {        
            cbType.Items.Add("+5 Dexterity Vest");
            cbType.Items.Add("Aged Brie");
            cbType.Items.Add("Elixir of the Mongoose");
            cbType.Items.Add("Sulfuras, Hand of Ragnaros");
            cbType.Items.Add("Backstage passes to a TAFKAL80ETC concert");
            cbType.Text = "Choose an item";
        }

        private void cbType_SelectedIndexChanged(object sender, EventArgs e)
        {
            if(cbType.SelectedItem.ToString() == "Sulfuras, Hand of Ragnaros")
            {
                nudlQuality.Maximum = 80;
                nudlQuality.Minimum = 80;
                nudlQuality.Value = 80;
            } else
            {
                nudlQuality.Minimum = 0;
                nudlQuality.Maximum = 50;
            }
        }
    }
}
