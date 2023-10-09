package com.gildedrose;

class GildedRose {
    Item[] items;

    public GildedRose(Item[] items) {
        this.items = items;
    }

    public void updateQuality() {
        for (Item item : items) {
            // "Aged Brie"와 "Backstage passes"가 아닌 경우
            // 품질이 0보다 크면 "Sulfuras, Hand of Ragnaros" 제외 아이템 품질을 1 감소
            if (!item.name.equals("Aged Brie")
                    && !item.name.equals("Backstage passes to a TAFKAL80ETC concert")) {
                if (item.quality > 0) {
                    if (!item.name.equals("Sulfuras, Hand of Ragnaros")) {
                        item.quality = item.quality - 1;
                    }
                }
            } else {
                // "Aged Brie" 또는 "Backstage passes"인 경우 품질 50 미만이면 품질 1 증가
                // "Backstage passes"의 경우 품질이 50 미만이고 판매일이 11일 미만인 경우 추가로 1 증가
                // 판매일이 6일 미만인 경우 또한 추가로 1 증가
                if (item.quality < 50) {
                    item.quality = item.quality + 1;

                    if (item.name.equals("Backstage passes to a TAFKAL80ETC concert")) {
                        if (item.sellIn < 11) {
                            if (item.quality < 50) {
                                item.quality = item.quality + 1;
                            }
                        }

                        if (item.sellIn < 6) {
                            if (item.quality < 50) {
                                item.quality = item.quality + 1;
                            }
                        }
                    }
                }
            }

            // "Sulfuras, Hand of Ragnaros"가 아닌 경우 판매일을 1 줄임
            if (!item.name.equals("Sulfuras, Hand of Ragnaros")) {
                item.sellIn = item.sellIn - 1;
            }

            // 판매일이 지난 경우
            // "Aged Brie"가 아니고, "Backstage passes"가 아닌 경우 품질이 0 보다 크면 줄임
            // "Backstage passes"가 아닌 경우 품질을 0으로 변경
            // "Aged Brie"인 경우 품질이 50 미만이면 증가
            if (item.sellIn < 0) {
                if (!item.name.equals("Aged Brie")) {
                    if (!item.name.equals("Backstage passes to a TAFKAL80ETC concert")) {
                        if (item.quality > 0) {
                            if (!item.name.equals("Sulfuras, Hand of Ragnaros")) {
                                item.quality = item.quality - 1;
                            }
                        }
                    } else {
                        item.quality = item.quality - item.quality;
                    }
                } else {
                    if (item.quality < 50) {
                        item.quality = item.quality + 1;
                    }
                }
            }
        }
    }
}
