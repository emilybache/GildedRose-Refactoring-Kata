using System.Collections.Generic;
using GildedRoseKata;

namespace GildedRoseTests;

public static class TestHelper
{
    public static void UpdateQualityForDays(GildedRose app, int days)
    {
        for (int i = 0; i < days; i++)
        {
            app.UpdateQuality();
        }
    }

    public static class ItemFactory
    {
        private static Item CreateItem(string name, int sellIn, int quality)
        {
            return new Item { Name = name, SellIn = sellIn, Quality = quality };
        }

        public static Item RegularItem(int sellIn = 10, int quality = 20)
            => CreateItem(ItemNames.RegularItem, sellIn, quality);

        public static Item AgedBrie(int sellIn = 10, int quality = 20)
            => CreateItem(ItemNames.AgedBrie, sellIn, quality);

        public static Item Sulfuras(int sellIn = 0)
            => CreateItem(ItemNames.Sulfuras, sellIn, QualityConstants.SulfurasQuality);

        public static Item BackstagePasses(int sellIn = 10, int quality = 20)
            => CreateItem(ItemNames.BackstagePasses, sellIn, quality);

        public static Item CreateAtQualityThreshold(string itemType, int sellIn, bool atMaximum)
        {
            var quality = atMaximum ? QualityConstants.MaxQuality : QualityConstants.MaxQuality - 1;
            return CreateItem(itemType, sellIn, quality);
        }
    }

    public static class TestDataBuilder
    {
        public static List<Item> CreateMixedInventory()
        {
            return new List<Item>
            {
                ItemFactory.RegularItem(),
                ItemFactory.AgedBrie(),
                ItemFactory.BackstagePasses()
            };
        }
    }
}
