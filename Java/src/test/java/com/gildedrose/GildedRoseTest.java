package com.gildedrose;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

@DisplayName("Gilded Rose 테스트")
class GildedRoseTest {
    private Item[] items;
    private GildedRose app;

    private void setup(String name, int sellIn, int quality) {
        items = new Item[]{new Item(name, sellIn, quality)};
        app = new GildedRose(items);
    }

    private int itemSellIn() {
        return app.items[0].sellIn;
    }

    private int itemQuality() {
        return app.items[0].quality;
    }

    @Nested
    @DisplayName("일반 아이템은")
    class StandardItem {

        @Test
        @DisplayName("하루가 지나면 Quality와 sellIn이 1씩 감소한다")
        void qualityAndSellInDecrease() {
            GildedRoseTest.this.setup("Standard Item", 10, 20);
            app.updateQuality();
            assertThat(itemQuality()).isEqualTo(19);
            assertThat(itemSellIn()).isEqualTo(9);
        }
    }

    @Nested
    @DisplayName("'Aged Brie'는")
    class AgedBrie {
        @Test
        @DisplayName("하루가 지나면 Quality는 증가하지만 sellIn은 감소한다")
        void qualityIncreasesSellInDecreases() {
            GildedRoseTest.this.setup("Aged Brie", 10, 20);
            app.updateQuality();
            assertThat(itemQuality()).isEqualTo(21);
            assertThat(itemSellIn()).isEqualTo(9);
        }
    }

    @Nested
    @DisplayName("'Backstage passes'는")
    class BackstagePasses {
        @Test
        @DisplayName("10일 초과 남았을 때 하루가 지나면 품질이 1 씩 증가하고 sellIn은 감소한다")
        void quality1IncreasesSellInDecreasesMoreThan10Days() {
            GildedRoseTest.this.setup("Backstage passes to a TAFKAL80ETC concert", 15, 20);
            app.updateQuality();
            assertThat(itemQuality()).isEqualTo(21);
            assertThat(itemSellIn()).isEqualTo(14);
        }

        @Test
        @DisplayName("6일에서 10일 남았을 때 하루가 지나면 품질이 2 씩 증가하고 sellIn은 감소한다")
        void quality2IncreasesSellInDecreases6to10Days() {
            GildedRoseTest.this.setup("Backstage passes to a TAFKAL80ETC concert", 10, 20);
            app.updateQuality();
            assertThat(itemQuality()).isEqualTo(22);
            assertThat(itemSellIn()).isEqualTo(9);
        }

        @Test
        @DisplayName("5일 미만 남았을 때 하루가 지나면 품질이 3 씩 증가하고 sellIn은 감소한다")
        void quality3IncreasesSellInDecreasesLessThan6Days() {
            GildedRoseTest.this.setup("Backstage passes to a TAFKAL80ETC concert", 5, 20);
            app.updateQuality();
            assertThat(itemQuality()).isEqualTo(23);
            assertThat(itemSellIn()).isEqualTo(4);
        }

        @Test
        @DisplayName("콘서트가 끝나면(즉, sellIn이 0 이하가 되면) 품질은 0이된다.")
        void qualityDropsToZeroAfterTheConcert() {
            GildedRoseTest.this.setup("Backstage passes to a TAFKAL80ETC concert", 0, 20);
            app.updateQuality();
            assertThat(itemQuality()).isEqualTo(0);
        }
    }

    @Nested
    @DisplayName("'Sulfuras'는")
    class Sulfuras {

        @Test
        @DisplayName("하루가 지나도 품질과 sellIn은 변하지 않는다")
        void qualityAndSellInUnchanged() {
            GildedRoseTest.this.setup("Sulfuras, Hand of Ragnaros", 10, 80);
            app.updateQuality();
            assertThat(itemQuality()).isEqualTo(80);
            assertThat(itemSellIn()).isEqualTo(10);
        }
    }
}
