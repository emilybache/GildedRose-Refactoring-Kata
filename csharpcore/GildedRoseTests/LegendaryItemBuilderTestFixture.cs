using System;
using FluentAssertions;
using GildedRoseKata;
using NUnit.Framework;

namespace GildedRoseTests;

public class LegendaryItemBuilderTestFixture
{

    [TestCase("Aged Brie", 3)]
    [TestCase("Elixir of the Mongoose", 0)]
    [TestCase("Backstage passes to a TAFKAL80ETC concert", -3)]
    public void WhenGivenOtherItemTypes_ShouldThrow(string name, int sellIn)
    {
        var action = () => new LegendaryItemBuilder(name, sellIn).Build();
        action.Should().Throw<Exception>().WithMessage("Only Legendary items can be built using this builder");
    }
    
    
    [TestCase("Sulfuras, Hand of Ragnaros", 3)]
    [TestCase("Sulfuras, Hand of Ragnaros", 0)]
    [TestCase("something sulfuras", -3)]
    public void WhenGivenValidParameters_ShouldBuildExpectedItemSuccessfully(string name, int sellIn)
    {
        var item =  new LegendaryItemBuilder(name, sellIn).Build(); 
        item.Name.Should().Be(name);
        item.SellIn.Should().Be(sellIn);
        item.Quality.Should().Be(ItemQuality.LegendaryItemQuality);
    }
    
}