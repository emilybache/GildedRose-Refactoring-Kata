using System;
using FluentAssertions;
using GildedRoseKata;
using NUnit.Framework;

namespace GildedRoseTests;

public class ItemBuilderTestFixture
{
    [TestCase("Aged Brie", 5, 0)]
    [TestCase("Elixir of the Mongoose", 0, 5)]
    [TestCase("Backstage passes to a TAFKAL80ETC concert", -5, 39)]
    public void WhenGivenValidParameters_ShouldBuildExpectedItemSuccessfully(string name, int sellIn,
        int quality)
    {
        var item = new ItemBuilder(name, sellIn, quality).Build();
        item.Name.Should().Be(name);
        item.SellIn.Should().Be(sellIn);
        item.Quality.Should().Be(quality);
    }


    [TestCase("Aged Brie", 5, -1)]
    [TestCase("Elixir of the Mongoose", 0, 51)]
    public void WhenGivenInvalidQuality_ShouldThrow(string name, int sellIn, int quality)
    {
        var action = () => new ItemBuilder(name, sellIn, quality).Build();
        action.Should().Throw<Exception>().WithMessage("An item's quality must be >=0 and <=50");
    }


    [TestCase("Sulfuras, Hand of Ragnaros", 3, 3)]
    [TestCase("something sulfuras", 3, 3)]
    public void WhenGivenLegendaryItem_ShouldThrow(string name, int sellIn, int quality)
    {
        var action = () => new ItemBuilder(name, sellIn, quality).Build();
        action.Should().Throw<Exception>().WithMessage("Legendary Items cannot be constructed using this builder");
    }

}