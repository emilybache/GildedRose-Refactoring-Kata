using System.Runtime.CompilerServices;

namespace csharp
{
    public class Item : IItem
    {
        public string Name { get; }
        public int SellIn { get; set; }
        public int Quality { get; set; }
        internal ItemType TypeOfItem;

        public Item(string name, int sellIn, int quality)
        {
            Name = name;
            SellIn = sellIn;
            Quality = quality;
            TypeOfItem = ItemType.Normal;
        }

        protected virtual void QualityModifier()
        {
            if (this.SellIn >= 0) SetQuality(1);
            else SetQuality(2);

        }
        
        protected virtual void SetQuality(int qualityModifier)
        {
            if (this.Quality - qualityModifier < 0) this.Quality = 0;
            else this.Quality -= qualityModifier;
        }

        public override string ToString()
        {
            return this.Name + ", " + this.SellIn + ", " + this.Quality;
        }

        public void PassingDay()
        {
            this.SellIn--;
            QualityModifier();
        }

        public int GetQuality()
        {
            return this.Quality;
        }
    }
}
