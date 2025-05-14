package com.gildedrose;

import com.gildedrose.enums.ItemName;
import com.gildedrose.service.AgedBrieQualityUpdater;
import com.gildedrose.service.BackstagePassesQualityUpdater;
import com.gildedrose.service.ConjuredQualityUpdater;
import com.gildedrose.service.GeneralQualityUpdater;
import com.gildedrose.service.QualityUpdater;
import lombok.Data;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import static com.gildedrose.enums.ItemName.AGED_BRIE;
import static com.gildedrose.enums.ItemName.BACKSTAGE_PASSES;
import static com.gildedrose.enums.ItemName.CONJURED;
import static com.gildedrose.enums.ItemName.GENERAL;

@Data
public class GildedRose {

    private static final Map<ItemName, QualityUpdater> qualityUpdaters =
        Map.of(AGED_BRIE, new AgedBrieQualityUpdater(),
            BACKSTAGE_PASSES, new BackstagePassesQualityUpdater(),
            CONJURED, new ConjuredQualityUpdater(),
            GENERAL, new GeneralQualityUpdater());

    private final List<Item> items;

    public void updateQuality() {
        Set<ItemName> itemNames = items.stream().map(item -> ItemName.resolve(item.getName())).collect(Collectors.toSet());

        for (ItemName itemName : itemNames) {
            QualityUpdater qualityUpdater = null;

            switch (itemName) {
                case AGED_BRIE:
                    qualityUpdater = qualityUpdaters.get(AGED_BRIE);
                    break;
                case BACKSTAGE_PASSES:
                    qualityUpdater = qualityUpdaters.get(BACKSTAGE_PASSES);
                    break;
                case SULFURAS_HAND_RANGAROS:
                    // nothing has to be done in that case - this is immutable object
                    break;
                case CONJURED:
                    qualityUpdater = qualityUpdaters.get(CONJURED);
                    break;
                default:
                    qualityUpdater = qualityUpdaters.get(GENERAL);
            }

            if (qualityUpdater != null) {
                List<Item> properItems =
                        items.stream().filter(item -> itemName.equals(ItemName.resolve(item.getName()))).collect(Collectors.toList());
                qualityUpdater.updateQuality(properItems);
            }
        }
    }
}
