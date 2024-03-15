using FluentAssertions;
using System.Collections.Generic;
using GildedRoseKata;
using NUnit.Framework;

namespace GildedRoseTests;

public class UpdateQualityTestFixture
{
  
    [Test]
    public void PlainItems_WhenNotExpired_Should_DecreaseSellInAndQualityByOne()
    {
        var items = new List<Item> { new ItemBuilder("item 1", 2, 3).Build() };
        var expectedItemsAfterTest = new List<Item> { new ItemBuilder("item 1", 1, 2).Build() };
        
        var app = new GildedRose(items);
        app.UpdateQuality();
        
        items.Should().BeEquivalentTo(expectedItemsAfterTest);
    }
    
    
    [Test]
    public void PlainItems_WhenExpired_Should_DecreaseSellInByOneAndQualityByTwo()
    {
        var items = new List<Item> { new ItemBuilder("item 1", 0, 5).Build() };
        var expectedItemsAfterDay1 = new List<Item> { new ItemBuilder("item 1", -1, 3).Build() };
        var expectedItemsAfterDay2 = new List<Item> { new ItemBuilder("item 1", -2, 1).Build() };
        
        
        var app = new GildedRose(items);
        
        app.UpdateQuality();
        items.Should().BeEquivalentTo(expectedItemsAfterDay1);
        
        app.UpdateQuality();
        items.Should().BeEquivalentTo(expectedItemsAfterDay2);
    }
    
    [Test]
    public void ItemsQuality_Should_NeverGoBelow0()
    {
        var items = new List<Item>
        {
            new ItemBuilder("item 1", 0, 0).Build(),
            new ItemBuilder("item 1", 0, 1).Build()
        };
        var expectedItemsAfterTest = new List<Item>
        {
            new ItemBuilder("item 1", -1, 0).Build(),
            new ItemBuilder("item 1", -1, 0).Build()
        };
        
        
        var app = new GildedRose(items);
        app.UpdateQuality();
        
        items.Should().BeEquivalentTo(expectedItemsAfterTest);
    }
    
    
    [Test]
    public void BetterWithAgeItems_WhenNotExpired_Should_IncreaseQualityByOne()
    {
        var items = new List<Item> { new ItemBuilder("Aged Brie", 1, 0).Build() };
        var expectedItemsAfterTest = new List<Item> { new ItemBuilder("Aged Brie", 0, 1).Build() };
        
        var app = new GildedRose(items);
        app.UpdateQuality();
        
        items.Should().BeEquivalentTo(expectedItemsAfterTest);
    }
    
    [Test]
    public void BetterWithAgeItems_WhenExpired_Should_IncreaseQualityByTwo()
    {
        var items = new List<Item> { new ItemBuilder("aged Brie", 0, 1).Build() };
        var expectedItemsAfterTest = new List<Item> { new ItemBuilder("aged Brie", -1, 3).Build() };
        
        var app = new GildedRose(items);
        app.UpdateQuality();
        
        items.Should().BeEquivalentTo(expectedItemsAfterTest);
    }
    
    
    [Test]
    public void ItemsQuality_Should_NeverIncreaseAbove50()
    {
        var items = new List<Item> { new ItemBuilder("Aged Brie", 1, 49).Build() };
        var expectedItemsAfterDay1 = new List<Item> { new ItemBuilder("Aged Brie", 0, 50).Build() };
        var expectedItemsAfterDay2 = new List<Item> { new ItemBuilder("Aged Brie", -1, 50).Build() };
        
        var app = new GildedRose(items);
        
        app.UpdateQuality();
        items.Should().BeEquivalentTo(expectedItemsAfterDay1);
        
        app.UpdateQuality();
        items.Should().BeEquivalentTo(expectedItemsAfterDay2);
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
    public void LegendaryItems_Quality_IsConstant80()
    {
        var items = new List<Item> { new LegendaryItemBuilder("something sulfuras something", 3).Build() };
        var expectedItemsAfterTest = new List<Item> { new LegendaryItemBuilder("something sulfuras something", 3).Build() };
       
        var app = new GildedRose(items);
        
        app.UpdateQuality();
        items.Should().BeEquivalentTo(expectedItemsAfterTest);
    }
    
    
    [Test]
    public void BackstagePassesItems_WhenExpiryInOver10Days_QualityIncreaseByOne()
    {
        var items = new List<Item>
        {
            new ItemBuilder("Backstage passes to a TAFKAL80ETC concert", 11, 4).Build(),
            new ItemBuilder("backstage passes to some other show", 20, 40).Build()
        };
        var expectedItemsAfterTest = new List<Item>
        {
            new ItemBuilder("Backstage passes to a TAFKAL80ETC concert", 10, 5).Build(),
            new ItemBuilder("backstage passes to some other show", 19, 41).Build()
        };
       
        var app = new GildedRose(items);
        
        app.UpdateQuality();
        items.Should().BeEquivalentTo(expectedItemsAfterTest);
    }

    
    [Test]
    public void BackstagePassesItems_WhenExpiryIn10To6Days_QualityIncreaseByTwo()
    {
        var items = new List<Item>
        {
            new ItemBuilder("backstage passes to a TAFKAL80ETC concert", 10, 4).Build(),
            new ItemBuilder("Backstage passes to some other show", 6, 41).Build()
        };
        var expectedItemsAfterTest = new List<Item>
        {
            new ItemBuilder("backstage passes to a TAFKAL80ETC concert", 9, 6).Build(),
            new ItemBuilder("Backstage passes to some other show", 5, 43).Build()
        };
       
        var app = new GildedRose(items);
        
        app.UpdateQuality();
        items.Should().BeEquivalentTo(expectedItemsAfterTest);
    }
   
    [Test]
    public void BackstagePassesItems_WhenExpiryIn5to1Days_QualityIncreaseByThree()
    {
        var items = new List<Item>
        {
            new ItemBuilder("Backstage passes to some other show", 5, 4).Build(),
            new ItemBuilder("Backstage passes to a TAFKAL80ETC concert", 1, 41).Build()
        };
        var expectedItemsAfterTest = new List<Item>
        {
            new ItemBuilder("Backstage passes to some other show", 4, 7).Build(),
            new ItemBuilder("Backstage passes to a TAFKAL80ETC concert", 0, 44).Build()
        };
       
        var app = new GildedRose(items);
        
        app.UpdateQuality();
        items.Should().BeEquivalentTo(expectedItemsAfterTest);
    }
    
    [Test]
    public void BackstagePassesItems_MaximumQuality_Is50()
    {
        var items = new List<Item>
        {
            new ItemBuilder("Backstage passes", 15, 49).Build(),
            new ItemBuilder("Backstage passes to some other show", 10,49).Build(),
            new ItemBuilder("Backstage passes to a TAFKAL80ETC concert", 5, 48).Build()
        };
        var expectedItemsAfterDay1 = new List<Item>
        {
            new ItemBuilder("Backstage passes", 14, 50).Build(),
            new ItemBuilder("Backstage passes to some other show", 9, 50).Build(),
            new ItemBuilder("Backstage passes to a TAFKAL80ETC concert",  4, 50).Build()
        };
        var expectedItemsAfterDay2 = new List<Item>
        {
            new ItemBuilder("Backstage passes",  13, 50).Build(),
            new ItemBuilder("Backstage passes to some other show", 8, 50).Build(),
            new ItemBuilder("Backstage passes to a TAFKAL80ETC concert",  3, 50).Build()
        };
       
        var app = new GildedRose(items);
        
        app.UpdateQuality();
        items.Should().BeEquivalentTo(expectedItemsAfterDay1);
        
        app.UpdateQuality();
        items.Should().BeEquivalentTo(expectedItemsAfterDay2);
    }
    
    [Test]
    public void BackstagePassesItems_WhenExpired_QualityDropsTo0()
    {
        var items = new List<Item>
        {
            new ItemBuilder("Backstage passes to a TAFKAL80ETC concert", 0, 4).Build(),
            new ItemBuilder("Another type of backstage passes", 0, 4).Build()
        };
        var expectedItemsAfterTest = new List<Item>
        {
            new ItemBuilder("Backstage passes to a TAFKAL80ETC concert", -1, 0).Build(),
            new ItemBuilder("Another type of backstage passes", -1, 0).Build()
        };
       
        var app = new GildedRose(items);
        
        app.UpdateQuality();
        items.Should().BeEquivalentTo(expectedItemsAfterTest);
    }
}