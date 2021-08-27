using System;
using Xunit;
using System.Collections.Generic;
using System.Reflection;
using GildedRose.Models;
using GildedRoseKata;

namespace GildedRoseTests
{
    public class GildedRoseTest
    {
        [Fact]
        public void foo()
        {
            IList<Item> items = new List<Item> { new AgedBrie() { Name = "fixme", SellIn = 0, Quality = 0 } };

            GildedRoseKata.GildedRose app = new GildedRoseKata.GildedRose(items);

            foreach (Item t in items)
            {
                Type type = t.GetType();
                var instance = Activator.CreateInstance(type);

                // Get a property on the type that is stored in the
                // property string
                PropertyInfo propSellDaysGone = type.GetProperty("SellDaysGone");
                PropertyInfo propSellIn = type.GetProperty("SellIn");
                PropertyInfo propQuality = type.GetProperty("Quality");
                PropertyInfo propName = type.GetProperty("Name");

                // Set the value of the given property on the given instance
                propSellDaysGone.SetValue(instance, 10, null);
                propSellIn.SetValue(instance, t.SellIn, null);
                propQuality.SetValue(instance, t.Quality, null);
                propName.SetValue(instance, t.Name, null);

                // Fetch the methods
                MethodInfo updateQualityMethod = type.GetMethod("UpdateQuality");
                MethodInfo updateSellinMethod = type.GetMethod("UpdateSellIn");

                // Invoke the respective implementation of the methods
                updateQualityMethod.Invoke(instance, new object[0]);
                updateSellinMethod.Invoke(instance, new object[0]);

                Assert.Equal("fixme", t.Name);
            }
        }
    }
}
