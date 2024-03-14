using System.Collections.Generic;

namespace GildedRoseKata;

public class GildedRose
{
    private readonly IList<Item> _items;
    private DailyUpdaterFactory _dailyUpdaterFactory;

    public GildedRose(IList<Item> items)
    {
        _items = items;
        // The DailyUpdaterFactory should have been injected through the constructor.
        // However, this will require changing the supporting tests which we don't want to do at this stage.
        _dailyUpdaterFactory = new DailyUpdaterFactory();
    }

    public void UpdateQuality()
    {
        foreach (var item in _items)
        {
            DailyUpdater dailyUpdater = _dailyUpdaterFactory.GetDailyUpdater(item);
            dailyUpdater.DailyUpdate(item);
        }
    }
    
}