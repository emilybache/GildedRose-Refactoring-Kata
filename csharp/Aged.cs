using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace csharp
{
    internal class Aged : Item
    {
        public Aged(string name, int sellIn, int quality) : base(name, sellIn, quality)
        {
            TypeOfItem = ItemType.Aged;
        }

        protected override void QualityModifier()
        {
            if (this.SellIn < 0) SetQuality(2);
            else SetQuality(1);
        }

        protected override void SetQuality(int qualityModifier)
        {
            if (this.Quality + qualityModifier > 50) this.Quality = 50;
            else this.Quality += qualityModifier;
        }
    }
}
