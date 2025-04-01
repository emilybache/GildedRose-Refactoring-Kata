﻿﻿﻿using System.Collections.Generic;
using GildedRoseKata;
using NUnit.Framework;

namespace GildedRoseTests;

public class GildedRoseTest
{
    [Test]
    public void StandardItem_QualityDegradesByOne()
    {
        var items = new List<Item> { new Item { Name = "Standard Item", SellIn = 5, Quality = 10 } };
        var app = new GildedRose(items);
        
        app.UpdateQuality();
        
        Assert.That(items[0].Quality, Is.EqualTo(9));
        Assert.That(items[0].SellIn, Is.EqualTo(4));
    }

    [Test]
    public void StandardItem_QualityDegradesTwiceAsFastAfterSellIn()
    {
        var items = new List<Item> { new Item { Name = "Standard Item", SellIn = 0, Quality = 10 } };
        var app = new GildedRose(items);
        
        app.UpdateQuality();
        
        Assert.That(items[0].Quality, Is.EqualTo(8));
    }

    [Test]
    public void AgedBrie_IncreasesInQuality()
    {
        var items = new List<Item> { new Item { Name = ItemCategory.AgedBrie, SellIn = 5, Quality = 10 } };
        var app = new GildedRose(items);
        
        app.UpdateQuality();
        
        Assert.That(items[0].Quality, Is.EqualTo(11));
    }

    [Test]
    public void AgedBrie_QualityNeverExceedsFifty()
    {
        var items = new List<Item> { new Item { Name = ItemCategory.AgedBrie, SellIn = 5, Quality = 50 } };
        var app = new GildedRose(items);
        
        app.UpdateQuality();
        
        Assert.That(items[0].Quality, Is.EqualTo(50));
    }

    [Test]
    public void Sulfuras_NeverChanges()
    {
        var items = new List<Item> { new Item { Name = ItemCategory.Sulfuras, SellIn = 5, Quality = 80 } };
        var app = new GildedRose(items);
        
        app.UpdateQuality();
        
        Assert.That(items[0].Quality, Is.EqualTo(80));
        Assert.That(items[0].SellIn, Is.EqualTo(5));
    }

    [Test]
    public void BackstagePasses_IncreasesInQuality()
    {
        var items = new List<Item> { new Item { Name = ItemCategory.BackstagePasses, SellIn = 15, Quality = 20 } };
        var app = new GildedRose(items);
        
        app.UpdateQuality();
        
        Assert.That(items[0].Quality, Is.EqualTo(21));
    }

    [Test]
    public void BackstagePasses_IncreasesByTwoWhenTenDaysOrLess()
    {
        var items = new List<Item> { new Item { Name = ItemCategory.BackstagePasses, SellIn = 10, Quality = 20 } };
        var app = new GildedRose(items);
        
        app.UpdateQuality();
        
        Assert.That(items[0].Quality, Is.EqualTo(22));
    }

    [Test]
    public void BackstagePasses_IncreasesByThreeWhenFiveDaysOrLess()
    {
        var items = new List<Item> { new Item { Name = ItemCategory.BackstagePasses, SellIn = 5, Quality = 20 } };
        var app = new GildedRose(items);
        
        app.UpdateQuality();
        
        Assert.That(items[0].Quality, Is.EqualTo(23));
    }

    [Test]
    public void BackstagePasses_QualityDropsToZeroAfterConcert()
    {
        var items = new List<Item> { new Item { Name = ItemCategory.BackstagePasses, SellIn = 0, Quality = 20 } };
        var app = new GildedRose(items);
        
        app.UpdateQuality();
        
        Assert.That(items[0].Quality, Is.EqualTo(0));
    }

    [Test]
    public void ConjuredItem_DegradesAtNormalRate()
    {
        var items = new List<Item> { new Item { Name = ItemCategory.Conjured, SellIn = 5, Quality = 20 } };
        var app = new GildedRose(items);
        
        app.UpdateQuality();
        
        Assert.That(items[0].Quality, Is.EqualTo(19));
    }

    [Test]
    public void ConjuredItem_DegradesAtNormalRateAfterSellIn()
    {
        var items = new List<Item> { new Item { Name = ItemCategory.Conjured, SellIn = 0, Quality = 20 } };
        var app = new GildedRose(items);
        
        app.UpdateQuality();
        
        Assert.That(items[0].Quality, Is.EqualTo(18));
    }

    [Test]
    public void QualityIsNeverNegative()
    {
        var items = new List<Item> 
        { 
            new Item { Name = "Standard Item", SellIn = 5, Quality = 0 },
            new Item { Name = ItemCategory.Conjured, SellIn = 5, Quality = 0 }
        };
        var app = new GildedRose(items);
        
        app.UpdateQuality();
        
        Assert.That(items[0].Quality, Is.EqualTo(0));
        Assert.That(items[1].Quality, Is.EqualTo(0));
    }
}
