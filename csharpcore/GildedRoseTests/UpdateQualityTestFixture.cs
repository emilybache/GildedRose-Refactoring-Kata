using System.Collections.Generic;
using FluentAssertions;
using GildedRoseKata;
using NUnit.Framework;

namespace GildedRoseTests;

public class UpdateQualityTestFixture
{
  
    [Test]
    public void PlainItems_WhenNotExpired_Should_DecreaseSellInAndQualityByOne()
    {
        var items = new List<Item> { new() { Name = "item 1", SellIn = 2, Quality = 3 } };
        var expectedItemsAfterTest = new List<Item> { new() { Name = "item 1", SellIn = 1, Quality = 2 } };
        
        var app = new GildedRose(items);
        app.UpdateQuality();
        
        items.Should().BeEquivalentTo(expectedItemsAfterTest);
    }
    
    
    [Test]
    public void PlainItems_WhenExpired_Should_DecreaseSellInByOneAndQualityByTwo()
    {
        var items = new List<Item> { new() { Name = "item 1", SellIn = 0, Quality = 5 } };
        var expectedItemsAfterDay1 = new List<Item> { new() { Name = "item 1", SellIn = -1, Quality = 3 } };
        var expectedItemsAfterDay2 = new List<Item> { new() { Name = "item 1", SellIn = -2, Quality = 1 } };
        
        
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
            new() { Name = "item 1", SellIn = 0, Quality = 0 },
            new() { Name = "item 1", SellIn = 0, Quality = 1 }
        };
        var expectedItemsAfterTest = new List<Item>
        {
            new() { Name = "item 1", SellIn = -1, Quality = 0 },
            new() { Name = "item 1", SellIn = -1, Quality = 0 }
        };
        
        
        var app = new GildedRose(items);
        app.UpdateQuality();
        
        items.Should().BeEquivalentTo(expectedItemsAfterTest);
    }
    
    
    [Test]
    public void AgedBrie_WhenNotExpired_Should_IncreaseQualityByOne()
    {
        var items = new List<Item> { new() { Name = "Aged Brie", SellIn = 1, Quality = 0 } };
        var expectedItemsAfterTest = new List<Item> { new() { Name = "Aged Brie", SellIn = 0, Quality = 1 } };
        
        var app = new GildedRose(items);
        app.UpdateQuality();
        
        items.Should().BeEquivalentTo(expectedItemsAfterTest);
    }
    
    [Test]
    public void AgedBrie_WhenExpired_Should_IncreaseQualityByTwo()
    {
        var items = new List<Item> { new() { Name = "Aged Brie", SellIn = 0, Quality = 1 } };
        var expectedItemsAfterTest = new List<Item> { new() { Name = "Aged Brie", SellIn = -1, Quality = 3 } };
        
        var app = new GildedRose(items);
        app.UpdateQuality();
        
        items.Should().BeEquivalentTo(expectedItemsAfterTest);
    }
    
    
    [Test]
    public void ItemsQuality_Should_NeverIncreaseAbove50()
    {
        var items = new List<Item> { new() { Name = "Aged Brie", SellIn = 1, Quality = 49 } };
        var expectedItemsAfterDay1 = new List<Item> { new() { Name = "Aged Brie", SellIn = 0, Quality = 50 } };
        var expectedItemsAfterDay2 = new List<Item> { new() { Name = "Aged Brie", SellIn = -1, Quality = 50 } };
        
        var app = new GildedRose(items);
        
        app.UpdateQuality();
        items.Should().BeEquivalentTo(expectedItemsAfterDay1);
        
        app.UpdateQuality();
        items.Should().BeEquivalentTo(expectedItemsAfterDay2);
    }
    
    
    [Test]
    public void SulfurasHandOfRagnaros_QualityAndSellIn_NeverChange()
    {
        var items = new List<Item> { new() { Name = "Sulfuras, Hand of Ragnaros", SellIn = 3, Quality = 3 } };
        var expectedItemsAfterTest = new List<Item> { new() { Name = "Sulfuras, Hand of Ragnaros", SellIn = 3, Quality = 3 } };
       
        var app = new GildedRose(items);
        
        app.UpdateQuality();
        items.Should().BeEquivalentTo(expectedItemsAfterTest);
    }
    
    [Test]
    [Ignore("Bug in code - support only item with exact name: Sulfuras, Hand of Ragnaros")]
    public void Sulfuras_QualityAndSellIn_NeverChange()
    {
        var items = new List<Item> { new() { Name = "Sulfuras", SellIn = 3, Quality = 3 } };
        var expectedItemsAfterTest = new List<Item> { new() { Name = "Sulfuras", SellIn = 3, Quality = 3 } };
       
        var app = new GildedRose(items);
        
        app.UpdateQuality();
        items.Should().BeEquivalentTo(expectedItemsAfterTest);
    }
    
    [Test]
    [Ignore("not implemented yet - Legendary Items should not take quality (constant at 80)")]
    public void Sulfuras_Quality_IsConstant80()
    {
        var items = new List<Item> { new() { Name = "Sulfuras", SellIn = 3 } };
        var expectedItemsAfterTest = new List<Item> { new() { Name = "Sulfuras", SellIn = 3, Quality = 80 } };
       
        var app = new GildedRose(items);
        
        app.UpdateQuality();
        items.Should().BeEquivalentTo(expectedItemsAfterTest);
    }
    
    
    [Test]
    public void BackstagePassesFullName_WhenExpiryInOver10Days_QualityIncreaseByOne()
    {
        var items = new List<Item>
        {
            new() { Name = "Backstage passes to a TAFKAL80ETC concert", SellIn = 11, Quality = 4 },
            new() { Name = "Backstage passes to a TAFKAL80ETC concert", SellIn = 20, Quality = 40 }
        };
        var expectedItemsAfterTest = new List<Item>
        {
            new() { Name = "Backstage passes to a TAFKAL80ETC concert", SellIn = 10, Quality = 5 },
            new() { Name = "Backstage passes to a TAFKAL80ETC concert", SellIn = 19, Quality = 41 }
        };
       
        var app = new GildedRose(items);
        
        app.UpdateQuality();
        items.Should().BeEquivalentTo(expectedItemsAfterTest);
    }

    
    [Test]
    public void BackstagePassesFullName_WhenExpiryIn10To6Days_QualityIncreaseByTwo()
    {
        var items = new List<Item>
        {
            new() { Name = "Backstage passes to a TAFKAL80ETC concert", SellIn = 10, Quality = 4 },
            new() { Name = "Backstage passes to a TAFKAL80ETC concert", SellIn = 6, Quality = 41 }
        };
        var expectedItemsAfterTest = new List<Item>
        {
            new() { Name = "Backstage passes to a TAFKAL80ETC concert", SellIn = 9, Quality = 6 },
            new() { Name = "Backstage passes to a TAFKAL80ETC concert", SellIn = 5, Quality = 43 }
        };
       
        var app = new GildedRose(items);
        
        app.UpdateQuality();
        items.Should().BeEquivalentTo(expectedItemsAfterTest);
    }
   
    [Test]
    public void BackstagePassesFullName_WhenExpiryIn5to1Days_QualityIncreaseByThree()
    {
        var items = new List<Item>
        {
            new() { Name = "Backstage passes to a TAFKAL80ETC concert", SellIn = 5, Quality = 4 },
            new() { Name = "Backstage passes to a TAFKAL80ETC concert", SellIn = 1, Quality = 41 }
        };
        var expectedItemsAfterTest = new List<Item>
        {
            new() { Name = "Backstage passes to a TAFKAL80ETC concert", SellIn = 4, Quality = 7 },
            new() { Name = "Backstage passes to a TAFKAL80ETC concert", SellIn = 0, Quality = 44 }
        };
       
        var app = new GildedRose(items);
        
        app.UpdateQuality();
        items.Should().BeEquivalentTo(expectedItemsAfterTest);
    }
    
    [Test]
    public void BackstagePassesFullName_MaximumQuality_Is50()
    {
        var items = new List<Item>
        {
            new() { Name = "Backstage passes to a TAFKAL80ETC concert", SellIn = 15, Quality = 49 },
            new() { Name = "Backstage passes to a TAFKAL80ETC concert", SellIn = 10, Quality = 49 },
            new() { Name = "Backstage passes to a TAFKAL80ETC concert", SellIn = 5, Quality = 48 }
        };
        var expectedItemsAfterDay1 = new List<Item>
        {
            new() { Name = "Backstage passes to a TAFKAL80ETC concert", SellIn = 14, Quality = 50 },
            new() { Name = "Backstage passes to a TAFKAL80ETC concert", SellIn = 9, Quality = 50 },
            new() { Name = "Backstage passes to a TAFKAL80ETC concert", SellIn = 4, Quality = 50 }
        };
        var expectedItemsAfterDay2 = new List<Item>
        {
            new() { Name = "Backstage passes to a TAFKAL80ETC concert", SellIn = 13, Quality = 50 },
            new() { Name = "Backstage passes to a TAFKAL80ETC concert", SellIn = 8, Quality = 50 },
            new() { Name = "Backstage passes to a TAFKAL80ETC concert", SellIn = 3, Quality = 50 }
        };
       
        var app = new GildedRose(items);
        
        app.UpdateQuality();
        items.Should().BeEquivalentTo(expectedItemsAfterDay1);
        
        app.UpdateQuality();
        items.Should().BeEquivalentTo(expectedItemsAfterDay2);
    }
    
    [Test]
    public void BackstagePassesFullName_WhenExpired_QualityDropsTo0()
    {
        var items = new List<Item> { new() { Name = "Backstage passes to a TAFKAL80ETC concert", SellIn = 0, Quality = 4 } };
        var expectedItemsAfterTest = new List<Item> { new() { Name = "Backstage passes to a TAFKAL80ETC concert", SellIn = -1, Quality = 0 } };
       
        var app = new GildedRose(items);
        
        app.UpdateQuality();
        items.Should().BeEquivalentTo(expectedItemsAfterTest);
    }
}