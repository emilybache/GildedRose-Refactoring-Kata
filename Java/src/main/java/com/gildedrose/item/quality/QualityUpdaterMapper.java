package com.gildedrose.item.quality;

import com.gildedrose.item.quality.type.QualifyUpdaterType;
import com.gildedrose.item.quality.updater.*;

import java.util.HashMap;
import java.util.Map;

public class QualityUpdaterMapper {
    private final Map<String, QualityUpdater> qualityUpdaters = new HashMap<>();
    private final static QualityUpdater DEFAULT_UPDATER = new QualityUpdaterDefault();

    public QualityUpdaterMapper() {
        qualityUpdaters.put(QualifyUpdaterType.AGED_BRIE.getValue().toUpperCase(), new QualityUpdaterAgedBrie());
        qualityUpdaters.put(QualifyUpdaterType.BACKSTAGE_PASSES.getValue().toUpperCase(), new QualityUpdaterBackstagePasses());
        qualityUpdaters.put(QualifyUpdaterType.CONJURED.getValue().toUpperCase(), new QualityUpdaterConjured());
        qualityUpdaters.put(QualifyUpdaterType.SULFURAS.getValue().toUpperCase(), new QualityUpdaterSulfuras());
    }

    public QualityUpdater getQualityUpdater(final String itemName) {
        return qualityUpdaters.getOrDefault(itemName.toUpperCase(), DEFAULT_UPDATER);
    }
}
