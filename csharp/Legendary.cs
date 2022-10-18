using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace csharp
{
    internal class Legendary : Item
    {
        public Legendary(string name, int sellIn, int quality) : base(name, sellIn, quality)
        {
            TypeOfItem = ItemType.Legendary;
        }

        protected override void QualityModifier()
        {
            //legendary items doesnt change!
            this.SellIn++;
        }
    }
}
