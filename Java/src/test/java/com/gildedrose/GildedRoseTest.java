package com.gildedrose;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class GildedRoseTest {

    @Test
    void foo() {
        Item[] items = new Item[] { new Item("foo", 0, 0) };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals("foo", app.items[0].name);
    }

    @Test
    void qualityAgedBrieNaoDevePassar50() {
        Item[] items = new Item[] { new Item("Aged Brie",0, 50 )};
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(50, app.items[0].quality, "O aged Brie nao deve passar 50 de quality");
    }

    @Test
    void AgedBrieDiminiuSellIn_QualityAumenta() {
        Item[] items = new Item[] { new Item("Aged Brie",10, 45 )};
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(46, app.items[0].quality, "O aged Brie deve aumentar a quality");
    }

    @Test
    void aposSellInIgualZeroQualityAumentaDois() {
        Item[] items = new Item[] { new Item("Aged Brie",0, 48 )};
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(50, app.items[0].quality, "O aged Brie nao deve aumentar 2 de quality");
    }

    @Test
    void quality_SellInDeveContuinuarIguaisEmSufulras() {
        Item[] items = new Item[] { new Item("Sulfuras, Hand of Ragnaros",10, 80)};
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(10, app.items[0].sellIn, "Sulfuras deve continuar 10 de sellIn");
        assertEquals(80, app.items[0].quality, "Sulfuras deve passar 50 de quality");
    }

    @Test
    void qualityNaoDeveSerNegativa() {
        Item[] items = new Item[] { new Item("New Item",10, 0)};
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(0, app.items[0].quality, "Quality nao deve ser negativa");
    }

    @Test
    void itemComum_DeveDiminuirSellInEQualityEmUmPorDia() {
        Item[] items = new Item[] { new Item("The Hell",10, 20)};
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(9, app.items[0].sellIn, "Deve diminuir o SellIn um ponto");
        assertEquals(19, app.items[0].quality,"Deve diminuir o Quality um ponto");
    }

    @Test
    void aposSellInIgualZeroDiminuirDoisQuality() {
        Item[] items = new Item[] { new Item("The Hell",0, 20)};
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(18, app.items[0].quality,"Deve diminuir o Quality um ponto");
    }

    @Test
    void dimunuirSellInAumentarQualityTAFKAL80ETC() {
        Item[] items = new Item[] { new Item("Backstage passes to a TAFKAL80ETC concert",1, 49 )};
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(50, app.items[0].quality, "Quality deve aumentar");
    }

    @Test
    void aumentarQualityEm2SellInEntre6A10TAFKAL80ETC() {
        Item[] items = new Item[] { new Item("Backstage passes to a TAFKAL80ETC concert",6, 40),
                                    new Item("Backstage passes to a TAFKAL80ETC concert",10, 48)};
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(42, app.items[0].quality, "Quality deve aumentar 2 pontos");
        assertEquals(50, app.items[1].quality, "Quality deve aumentar 2 pontos");
    }

    @Test
    void deveAumentar3QualityQuandoSellInEntre1A5TAFKAL80ETC() {
        Item[] items = { new Item("Backstage passes to a TAFKAL80ETC concert",1, 3),
                         new Item("Backstage passes to a TAFKAL80ETC concert",5, 9)};
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(6, app.items[0].quality, "Quality deve 6");
        assertEquals(12, app.items[1].quality, "Quality deve ser 12");
    }

    @Test
    void qualityCairPra0QuandoSellInMenorZero() {
        Item[] items = { new Item("Backstage passes to a TAFKAL80ETC concert",0,10)};
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals(0, app.items[0].quality, "Quality deve cair pra 0");
    }
}
