using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using System.Linq.Expressions;
using System.Text;
using System.Threading.Tasks;

namespace csharp
{
    internal class BackstagePass : Item
    {
        public BackstagePass(string name, int sellIn, int quality) : base(name, sellIn, quality)
        {
            TypeOfItem = ItemType.BackstagePass;
        }

        protected override void QualityModifier()
        {
            if (this.SellIn < 0) this.Quality = 0;
            else SetQuality(SetQualityModifier());
        }

        protected override void SetQuality(int qualityModifier)
        {
            if (this.Quality + qualityModifier > 50) this.Quality = 50;
            else this.Quality += qualityModifier;
        }

        private int SetQualityModifier()
        {
            int temp = this.SellIn / 5;
            int result=1;
            switch(temp)
            {
                case int n when (n < 1):
                    result = 3;
                    break;
                case int n when(n < 2):
                    result = 2;
                    break;
                default:
                    break;
            }
            return result;
        }

    }
}
