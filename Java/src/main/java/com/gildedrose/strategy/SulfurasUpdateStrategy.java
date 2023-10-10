package com.gildedrose.strategy;

import com.gildedrose.Item;

public class SulfurasUpdateStrategy implements ItemUpdateStrategy {
    @Override
    public void update(Item item) {
        // "Sulfuras, Hand of Ragnaros"는 판매일과 품질이 변하지 않음
    }
}
