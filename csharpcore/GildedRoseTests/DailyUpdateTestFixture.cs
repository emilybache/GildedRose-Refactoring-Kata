using FluentAssertions;
using System.Collections.Generic;
using GildedRoseKata;
using NUnit.Framework;

namespace GildedRoseTests;

public class DailyUpdateTestFixture
{
   
    [Test]
    public void RegularItems_WhenNotExpired_Should_DecreaseSellInBy1AndQualityBy1DownTo0()
    {
        var items = new List<Item>
        {
            new ItemBuilder("item 1", 2, 3).Build(),
            new ItemBuilder("+5 Dexterity Vest", 2, 1).Build(),
            new ItemBuilder("item 1", 2, 0).Build()
        };
        var expectedItemsAfterTest = new List<Item>
        {
            new ItemBuilder("item 1", 1, 2).Build(),
            new ItemBuilder("+5 Dexterity Vest", 1, 0).Build(),
            new ItemBuilder("item 1", 1, 0).Build()
        };
        
        var app = new GildedRose(items);
        app.UpdateQuality();
        
        items.Should().BeEquivalentTo(expectedItemsAfterTest);
    }
    
    
    [Test]
    public void RegularItems_WhenExpired_Should_DecreaseSellInBy1AndQualityBy2DownTo0()
    {
        var items = new List<Item>
        {
            new ItemBuilder("item 1", -1, 3).Build(),
            new ItemBuilder("+5 Dexterity Vest", -1, 1).Build(),
            new ItemBuilder("item 1", -1, 0).Build()
        };
        var expectedItemsAfterTest = new List<Item>
        {
            new ItemBuilder("item 1", -2, 1).Build(),
            new ItemBuilder("+5 Dexterity Vest", -2, 0).Build(),
            new ItemBuilder("item 1", -2, 0).Build()
        };
        
        
        var app = new GildedRose(items);
        
        app.UpdateQuality();
        items.Should().BeEquivalentTo(expectedItemsAfterTest);
    }
    
    [Test]
    public void BetterWithAgeItems_WhenNotExpired_Should_DecreaseSellInBy1AndIncreaseQualityBy1Upto50()
    {
        var items = new List<Item>
        {
            new ItemBuilder("Aged Brie", 1, 0).Build(),
            new ItemBuilder("Aged Brie", 1, 49).Build(),
            new ItemBuilder("Aged Brie", 1, 50).Build()
            
        };
        var expectedItemsAfterTest = new List<Item>
        {
            new ItemBuilder("Aged Brie", 0, 1).Build(),
            new ItemBuilder("Aged Brie", 0, 50).Build(),
            new ItemBuilder("Aged Brie", 0, 50).Build()
            
        };
        
        var app = new GildedRose(items);
        app.UpdateQuality();
        
        items.Should().BeEquivalentTo(expectedItemsAfterTest);
    }
    
    [Test]
    public void BetterWithAgeItems_WhenExpired_Should_DecreaseSellInBy1AndIncreaseQualityBy2Upto50()
    {
        var items = new List<Item>
        {
            new ItemBuilder("aged Brie", 0, 1).Build(),
            new ItemBuilder("aged Brie", 0, 49).Build(),
            new ItemBuilder("aged Brie", 0, 50).Build()
        };
        var expectedItemsAfterTest = new List<Item>
        {
            new ItemBuilder("aged Brie", -1, 3).Build(),
            new ItemBuilder("aged Brie", -1, 50).Build(),
            new ItemBuilder("aged Brie", -1, 50).Build()
        };
        
        var app = new GildedRose(items);
        app.UpdateQuality();
        
        items.Should().BeEquivalentTo(expectedItemsAfterTest);
    }
    
    [Test]
    public void LegendaryItems_QualityAndSellIn_NeverChange()
    {
        var items = new List<Item>
        {
            new LegendaryItemBuilder("Sulfuras, Hand of Ragnaros", 3).Build(),
            new LegendaryItemBuilder("sulfuras", 3).Build()
        };
        var expectedItemsAfterTest = new List<Item>
        {
            new LegendaryItemBuilder("Sulfuras, Hand of Ragnaros", 3).Build(),
            new LegendaryItemBuilder("sulfuras", 3).Build()
        };
       
        var app = new GildedRose(items);
        
        app.UpdateQuality();
        items.Should().BeEquivalentTo(expectedItemsAfterTest);
    }
    
    [Test]
    public void BackstagePassesItems_WhenExpiryInOver10Days_DecreaseSellInBy1AndQualityIncreaseBy1Upto50()
    {
        var items = new List<Item>
        {
            new ItemBuilder("Backstage passes to a TAFKAL80ETC concert", 11, 4).Build(),
            new ItemBuilder("backstage passes to some other show", 20, 40).Build(),
            new ItemBuilder("backstage passes to some other show", 20, 49).Build(),
            new ItemBuilder("backstage passes to some other show", 20, 50).Build()
        };
        var expectedItemsAfterTest = new List<Item>
        {
            new ItemBuilder("Backstage passes to a TAFKAL80ETC concert", 10, 5).Build(),
            new ItemBuilder("backstage passes to some other show", 19, 41).Build(),
            new ItemBuilder("backstage passes to some other show", 19, 50).Build(),
            new ItemBuilder("backstage passes to some other show", 19, 50).Build()
        };
       
        var app = new GildedRose(items);
        
        app.UpdateQuality();
        items.Should().BeEquivalentTo(expectedItemsAfterTest);
    }

    
    [Test]
    public void BackstagePassesItems_WhenExpiryIn10To6Days_DecreaseSellInBy1AndQualityIncreaseBy2Upto50()
    {
        var items = new List<Item>
        {
            new ItemBuilder("backstage passes to a TAFKAL80ETC concert", 10, 4).Build(),
            new ItemBuilder("Backstage passes to some other show", 6, 41).Build(),
            new ItemBuilder("backstage passes to some other show", 10, 49).Build(),
            new ItemBuilder("backstage passes to some other show", 8, 50).Build()
        };
        var expectedItemsAfterTest = new List<Item>
        {
            new ItemBuilder("backstage passes to a TAFKAL80ETC concert", 9, 6).Build(),
            new ItemBuilder("Backstage passes to some other show", 5, 43).Build(),
            new ItemBuilder("backstage passes to some other show", 9, 50).Build(),
            new ItemBuilder("backstage passes to some other show", 7, 50).Build()
        };
       
        var app = new GildedRose(items);
        
        app.UpdateQuality();
        items.Should().BeEquivalentTo(expectedItemsAfterTest);
    }
   
    [Test]
    public void BackstagePassesItems_WhenExpiryIn5to1Days_DecreaseSellInBy1AndQualityIncreaseBy3Upto50()
    {
        var items = new List<Item>
        {
            new ItemBuilder("Backstage passes to some other show", 5, 4).Build(),
            new ItemBuilder("Backstage passes to a TAFKAL80ETC concert", 1, 41).Build(),
            new ItemBuilder("Backstage passes to a TAFKAL80ETC concert", 4, 48).Build(),
            new ItemBuilder("Backstage passes to a TAFKAL80ETC concert", 2, 49).Build(),
            new ItemBuilder("Backstage passes to a TAFKAL80ETC concert", 1, 50).Build()
        };
        var expectedItemsAfterTest = new List<Item>
        {
            new ItemBuilder("Backstage passes to some other show", 4, 7).Build(),
            new ItemBuilder("Backstage passes to a TAFKAL80ETC concert", 0, 44).Build(),
            new ItemBuilder("Backstage passes to a TAFKAL80ETC concert", 3, 50).Build(),
            new ItemBuilder("Backstage passes to a TAFKAL80ETC concert", 1, 50).Build(),
            new ItemBuilder("Backstage passes to a TAFKAL80ETC concert", 0, 50).Build()
        };
       
        var app = new GildedRose(items);
        
        app.UpdateQuality();
        items.Should().BeEquivalentTo(expectedItemsAfterTest);
    }
    
    [Test]
    public void BackstagePassesItems_WhenExpired_DecreaseSellInBy1AndQualityDropsTo0()
    {
        var items = new List<Item>
        {
            new ItemBuilder("Backstage passes to a TAFKAL80ETC concert", 0, 4).Build(),
            new ItemBuilder("Another type of backstage passes", -1, 1).Build(),
            new ItemBuilder("Another type of backstage passes", -16, 15).Build()
        };
        var expectedItemsAfterTest = new List<Item>
        {
            new ItemBuilder("Backstage passes to a TAFKAL80ETC concert", -1, 0).Build(),
            new ItemBuilder("Another type of backstage passes", -2, 0).Build(),
            new ItemBuilder("Another type of backstage passes", -17, 0).Build()
        };
       
        var app = new GildedRose(items);
        
        app.UpdateQuality();
        items.Should().BeEquivalentTo(expectedItemsAfterTest);
    }
    
    [Test]
    public void ConjuredItems_WhenNotExpired_Should_DecreaseSellInBy1AndQualityBy2DownTo0()
    {
        var items = new List<Item>
        {
            new ItemBuilder("Conjured Mana Cake", 1, 4).Build(),
            new ItemBuilder("some other conjured item", 15, 50).Build(),
            new ItemBuilder("some other conjured item", 5, 1).Build(),
            new ItemBuilder("some other conjured item", 5, 0).Build()
        };
        var expectedItemsAfterTest = new List<Item>
        {
            new ItemBuilder("Conjured Mana Cake", 0, 2).Build(),
            new ItemBuilder("some other conjured item", 14, 48).Build(),
            new ItemBuilder("some other conjured item", 4, 0).Build(),
            new ItemBuilder("some other conjured item", 4, 0).Build()
        };
        
        var app = new GildedRose(items);
        app.UpdateQuality();
        
        items.Should().BeEquivalentTo(expectedItemsAfterTest);
    }
    
    [Test]
    public void ConjuredItems_WhenExpired_Should_DecreaseSellInBy1AndQualityBy4DownTo0()
    {
        var items = new List<Item>
        {
            new ItemBuilder("Conjured Mana Cake", 0, 3).Build(),
            new ItemBuilder("Conjured Mana Cake", -1, 2).Build(),
            new ItemBuilder("Conjured Mana Cake", -5, 1).Build(),
            new ItemBuilder("Conjured Mana Cake", 0, 0).Build(),
            new ItemBuilder("some other conjured item", -1, 50).Build()
        };
        var expectedItemsAfterTest = new List<Item>
        {
            new ItemBuilder("Conjured Mana Cake", -1, 0).Build(),
            new ItemBuilder("Conjured Mana Cake", -2, 0).Build(),
            new ItemBuilder("Conjured Mana Cake", -6, 0).Build(),
            new ItemBuilder("Conjured Mana Cake", -1, 0).Build(),
            new ItemBuilder("some other conjured item", -2, 46).Build()
        };
        
        var app = new GildedRose(items);
        app.UpdateQuality();
        
        items.Should().BeEquivalentTo(expectedItemsAfterTest);
    }

}