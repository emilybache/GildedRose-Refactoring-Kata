package com.gildedrose;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

public class ItemUpdaterFactory {

    private static final Map<String, ItemUpdater> registeredCustomUpdaters = new HashMap<>();
    static {
        registeredCustomUpdaters.put("Aged Brie", new AgedBrieUpdater());
        registeredCustomUpdaters.put("Backstage passes to a TAFKAL80ETC concert", new BackstagePassUpdater());
        registeredCustomUpdaters.put("Sulfuras, Hand of Ragnaros", new SulfurasUpdater());
        registeredCustomUpdaters.put("Conjured Mana Cake", new ConjuredUpdater());
    }

    public static void registerCustomUpdater(String type, ItemUpdater updater ){
        registeredCustomUpdaters.put(type, updater);
    }

    public static ItemUpdater getItemUpdater(Item item) {
        return Optional.ofNullable(registeredCustomUpdaters.get(item.name))
                .orElse(new StandardItemUpdater());
    }
}
