using Xunit;
using System.Collections.Generic;
using GildedRoseKata;
using System.Diagnostics.Metrics;

namespace GildedRoseTests;

public class GildedRoseTest
{
    [Fact]
    public void foo()
    {
        IList<Item> Items = new List<Item> { new Item { Name = "foo", SellIn = 0, Quality = 0 } };
        GildedRose app = new GildedRose(Items);
        app.UpdateQuality();
        Assert.Equal("foo", Items[0].Name);
    }
    [Fact]
    public void givenSellInDateNegative_QualtiyDecreasesByTwo()
    {
        IList<Item> Items = new List<Item> { new Item { Name = "foo", SellIn = 0, Quality = 2 } };
        GildedRose app = new GildedRose(Items);
        app.UpdateQuality();
        Assert.Equal(0, Items[0].Quality);
    }

    [Fact]
    public void givenUpdate_QualityNeverLessThanZero ()
    {
        IList<Item> Items = new List<Item> { new Item { Name = "foo", SellIn = 0, Quality = 1 } };
        GildedRose app = new GildedRose(Items);
        app.UpdateQuality();
        Assert.Equal(0, Items[0].Quality);
    }

    //Aged Brie" actually increases in Quality the older it get
    [Fact]
    public void givenAgedBrie_QualityIncreases()
    {
        IList<Item> Items = new List<Item> { new Item { Name = "Aged Brie", SellIn = 1, Quality = 1 } };
        GildedRose app = new GildedRose(Items);
        app.UpdateQuality();
        Assert.Equal(2, Items[0].Quality);
    }
    //The Quality of an item is never more than 50
    [Fact]
    public void givenQualityUpdate_QualityNotGreaterThanFifty ()
    {
        IList<Item> Items = new List<Item> { new Item { Name = "Aged Brie", SellIn = 0, Quality = 50 } };
        GildedRose app = new GildedRose(Items);
        app.UpdateQuality();
        Assert.Equal(50, Items[0].Quality);
    }
    [Fact] // need to update this, quality needs to cap at 50
    public void givenAgedBrieAndSellInGreaterThanZero_QualityIncreasesByOne()
    {
        IList<Item> Items = new List<Item> { new Item { Name = "Aged Brie", SellIn = 10, Quality = 49 } };
        GildedRose app = new GildedRose(Items);
        app.UpdateQuality();
        Assert.Equal(50, Items[0].Quality);
    }

    //"Sulfuras", being a legendary item, never has to be sold or decreases in Quality

    [Fact]
    public void givenSulfuras_NeverSoldOrDecreasesInQuality ()
    {
        IList<Item> Items = new List<Item> { new Item { Name = "Sulfuras, Hand of Ragnaros", SellIn = 10, Quality = 80 } };
        GildedRose app = new GildedRose(Items);
        app.UpdateQuality();
        Assert.Equal(80, Items[0].Quality);
        Assert.Equal(10, Items[0].SellIn);
    }

//    "Backstage passes", like aged brie, increases in Quality as its SellIn value approaches;
//    Quality increases by 2 when there are 10 days or less

    [Fact]
    public void givenBackstagePasses_QualityIncreaseByTwoWhenTenDaysOrLess ()
    {
        IList<Item> Items = new List<Item> { new Item { Name = "Backstage passes to a TAFKAL80ETC concert", SellIn = 6, Quality = 6 } };
        GildedRose app = new GildedRose(Items);
        app.UpdateQuality();
        Assert.Equal(8, Items[0].Quality);

    }

    //    and by 3 when there are 5 days or less but

    [Fact]
    public void givenBackstagePasses_QualityIncreaseByThreeWhenFiveDaysOrLess()
    {
        IList<Item> Items = new List<Item> { new Item { Name = "Backstage passes to a TAFKAL80ETC concert", SellIn = 4, Quality = 6 } };
        GildedRose app = new GildedRose(Items);
        app.UpdateQuality();
        Assert.Equal(9, Items[0].Quality);

    }    
    //Quality drops to 0 after the concert

    [Fact]
    public void givenBackstagePasses_QualityIsZeroAfterConcert()
    {
        IList<Item> Items = new List<Item> { new Item { Name = "Backstage passes to a TAFKAL80ETC concert", SellIn = 0, Quality = 6 } };
        GildedRose app = new GildedRose(Items);
        app.UpdateQuality();
        Assert.Equal(0, Items[0].Quality);
    }

    ///"Conjured" items degrade in Quality twice as fast as normal items

    [Fact]
    public void givenConjuredItem_QualityDegradesTwiceAsFastAsNormal ()
    {
        IList<Item> Items = new List<Item> { new Item { Name = "Conjured Mana Cake", SellIn = 10, Quality = 10 } };
        GildedRose app = new GildedRose(Items);
        app.UpdateQuality();
        Assert.Equal(8, Items[0].Quality);
    }

}