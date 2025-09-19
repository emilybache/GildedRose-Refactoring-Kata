package com.gildedrose.strategy;

import com.gildedrose.Item;
import com.gildedrose.calculator.quality.impl.AgedBrieQualityCalculator;
import com.gildedrose.calculator.quality.impl.BackstagePassQualityCalculator;
import com.gildedrose.calculator.quality.impl.DefaultQualityCalculator;
import com.gildedrose.calculator.quality.impl.SulfurasQualityCalculator;
import com.gildedrose.calculator.sellIn.impl.StandardSellInCalculator;
import com.gildedrose.calculator.sellIn.impl.SulfurasSellInCalculator;
import com.gildedrose.strategy.impl.ItemUpdateStrategy;

import java.util.Map;

public class StrategyProvider {

    private final Map<String, UpdateStrategy> strategies;
    private final UpdateStrategy defaultStrategy;

    public static final String AGED_BRIE_NAME = "Aged Brie";
    public static final String BACKSTAGE_NAME = "Backstage passes to a TAFKAL80ETC concert";
    public static final String SULFURAS_NAME = "Sulfuras, Hand of Ragnaros";

    public StrategyProvider() {
        UpdateStrategy normalStrategy = new ItemUpdateStrategy(new DefaultQualityCalculator(), new StandardSellInCalculator());
        UpdateStrategy agedBrieStrategy = new ItemUpdateStrategy(new AgedBrieQualityCalculator(), new StandardSellInCalculator());
        Map<String, UpdateStrategy> strategyMap = getUpdateStrategyMap(agedBrieStrategy);
        this.strategies = strategyMap;
        this.defaultStrategy = normalStrategy;
    }

    public StrategyProvider(Map<String, UpdateStrategy> strategyMap, UpdateStrategy defaultStrategy) {
        this.strategies = strategyMap;
        this.defaultStrategy = defaultStrategy;
    }

    public UpdateStrategy getStrategyForItem(Item item) {
        return strategies.getOrDefault(item.name, defaultStrategy);
    }

    private static Map<String, UpdateStrategy> getUpdateStrategyMap(UpdateStrategy agedBrieStrategy) {
        UpdateStrategy backstageStrategy = new ItemUpdateStrategy(new BackstagePassQualityCalculator(), new StandardSellInCalculator());
        UpdateStrategy sulfurasStrategy = new ItemUpdateStrategy(new SulfurasQualityCalculator(), new SulfurasSellInCalculator());

        Map<String, UpdateStrategy> strategyMap = Map.of(
            AGED_BRIE_NAME, agedBrieStrategy,
            BACKSTAGE_NAME, backstageStrategy,
            SULFURAS_NAME, sulfurasStrategy
        );
        return strategyMap;
    }
}
