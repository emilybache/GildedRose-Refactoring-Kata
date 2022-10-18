using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace csharp
{
    internal class Conjured : Item
    {
        public Conjured(string name, int sellIn, int quality) : base(name, sellIn, quality)
        {
            TypeOfItem = ItemType.Conjured;
        }

        protected override void QualityModifier()
        {
            if (this.SellIn >= 0) SetQuality(2);
            else SetQuality(4);
        }
    }
}
