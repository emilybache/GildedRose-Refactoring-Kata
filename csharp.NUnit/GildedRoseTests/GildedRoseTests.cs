using System.Collections.Generic;
using GildedRoseKata;
using NUnit.Framework;

namespace GildedRoseTests;

[TestFixture]
public class GildedRoseTests
{
    [TestCase(10, 20, 19, Description = "Regular item before sell date")]
    [TestCase(0, 20, 18, Description = "Regular item on sell date")]
    [TestCase(-1, 20, 18, Description = "Regular item after sell date")]
    [TestCase(10, QualityConstants.MinQuality, QualityConstants.MinQuality,
        Description = "Regular item at minimum quality")]
    [TestCase(-5, QualityConstants.MinQuality, QualityConstants.MinQuality,
        Description = "Regular item at minimum quality after sell date")]
    public void RegularItem_QualityProgression(int sellIn, int quality, int expectedQuality)
    {
        // Arrange
        var items = new List<Item> { TestHelper.ItemFactory.RegularItem(sellIn, quality) };
        var app = new GildedRose(items);

        // Act
        app.UpdateQuality();

        // Assert
        Assert.Multiple(() =>
        {
            Assert.That(items[0].Quality, Is.EqualTo(expectedQuality),
                $"Quality should be {expectedQuality} after update");
            Assert.That(items[0].SellIn, Is.EqualTo(sellIn - 1),
                "SellIn should decrease by 1");
        });
    }

    [TestCase(5, 10, 11, Description = "Before sell date")]
    [TestCase(0, 10, 12, Description = "On sell date")]
    [TestCase(-1, 10, 12, Description = "After sell date")]
    [TestCase(5, QualityConstants.MaxQuality, QualityConstants.MaxQuality, Description = "At maximum quality")]
    [TestCase(5, QualityConstants.MaxQuality - 1, QualityConstants.MaxQuality,
        Description = "Approaching maximum quality")]
    [TestCase(-5, QualityConstants.MaxQuality - 2, QualityConstants.MaxQuality,
        Description = "Double increase near maximum")]
    public void AgedBrie_QualityProgression(int sellIn, int quality, int expectedQuality)
    {
        // Arrange
        var items = new List<Item> { TestHelper.ItemFactory.AgedBrie(sellIn, quality) };
        var app = new GildedRose(items);

        // Act
        app.UpdateQuality();

        // Assert
        Assert.Multiple(() =>
        {
            Assert.That(items[0].Quality, Is.EqualTo(expectedQuality));
            Assert.That(items[0].SellIn, Is.EqualTo(sellIn - 1));
        });
    }

    [TestCase(11, 20, 21, Description = ">10 days: +1")]
    [TestCase(10, 20, 22, Description = "10 days: +2")]
    [TestCase(5, 20, 23, Description = "5 days: +3")]
    [TestCase(0, 20, 0, Description = "On concert: drops to 0")]
    [TestCase(-1, 20, 0, Description = "After concert: remains 0")]
    [TestCase(5, QualityConstants.MaxQuality - 1, QualityConstants.MaxQuality, Description = "Approaching maximum")]
    [TestCase(5, QualityConstants.MaxQuality, QualityConstants.MaxQuality, Description = "At maximum")]
    public void BackstagePasses_QualityProgression(int sellIn, int quality, int expectedQuality)
    {
        // Arrange
        var items = new List<Item> { TestHelper.ItemFactory.BackstagePasses(sellIn, quality) };
        var app = new GildedRose(items);

        // Act
        app.UpdateQuality();

        // Assert
        Assert.Multiple(() =>
        {
            Assert.That(items[0].Quality, Is.EqualTo(expectedQuality));
            Assert.That(items[0].SellIn, Is.EqualTo(sellIn - 1));
        });
    }

    [Test]
    public void Sulfuras_NeverChanges()
    {
        // Arrange
        var items = new List<Item> { TestHelper.ItemFactory.Sulfuras() };
        var app = new GildedRose(items);

        // Act
        TestHelper.UpdateQualityForDays(app, 10); // Test multiple days

        // Assert
        Assert.Multiple(() =>
        {
            Assert.That(items[0].Quality, Is.EqualTo(QualityConstants.SulfurasQuality));
            Assert.That(items[0].SellIn, Is.EqualTo(0));
        });
    }

    [Test]
    public void MultipleItems_UpdatedCorrectly()
    {
        // Arrange
        var items = TestHelper.TestDataBuilder.CreateMixedInventory();
        items.Add(TestHelper.ItemFactory.Sulfuras()); // Add Sulfuras to test all item types
        var app = new GildedRose(items);

        // Act
        app.UpdateQuality();

        // Assert
        Assert.Multiple(() =>
        {
            Assert.That(items[0].Quality, Is.EqualTo(19), "Regular item");
            Assert.That(items[1].Quality, Is.EqualTo(21), "Aged Brie");
            Assert.That(items[2].Quality, Is.EqualTo(22), "Backstage passes");
            Assert.That(items[3].Quality, Is.EqualTo(QualityConstants.SulfurasQuality), "Sulfuras");
        });
    }

    [Test]
    public void QualityThresholds_HandledCorrectly()
    {
        // Arrange
        var items = new List<Item>
        {
            TestHelper.ItemFactory.CreateAtQualityThreshold(ItemNames.AgedBrie, 5, true),
            TestHelper.ItemFactory.CreateAtQualityThreshold(ItemNames.BackstagePasses, 5, false)
        };
        var app = new GildedRose(items);

        // Act
        app.UpdateQuality();

        // Assert
        Assert.Multiple(() =>
        {
            Assert.That(items[0].Quality, Is.EqualTo(QualityConstants.MaxQuality), "Aged Brie at max");
            Assert.That(items[1].Quality, Is.EqualTo(QualityConstants.MaxQuality), "Backstage passes reaching max");
        });
    }

    [Test]
    public void EmptyInventory_HandledGracefully()
    {
        // Arrange
        var app = new GildedRose(new List<Item>());

        // Act & Assert
        Assert.DoesNotThrow(() => TestHelper.UpdateQualityForDays(app, 5),
            "Should handle empty inventory for multiple days");
    }
}
