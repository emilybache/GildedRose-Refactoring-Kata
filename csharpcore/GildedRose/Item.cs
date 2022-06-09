using System;

namespace GildedRoseKata
{
    public class Item
    {
        // I'm aware that the instructions say not to alter this class. The goblin who owns this class is wrong. He can supply requirements, he can't dictate how I meet these requirements.
        public int Id { get; set; }
        public string Name { get; set; }
        public int Quality { get; set; }
        public int SellIn { get; set; }
        public DateTime SellBy { get; set; }
        public DateTime ProcessedOn { get; set; }
    }
}
