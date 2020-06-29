package com.gildedrose.item;

import com.gildedrose.Item;
import com.gildedrose.item.*;
import org.junit.jupiter.api.Test;

import static com.gildedrose.item.GRItemFactory.*;
import static org.apache.commons.lang3.RandomStringUtils.random;
import static org.assertj.core.api.Assertions.assertThat;

public class GRItemFactoryCreateTest {

    @Test
    public void createsRegularGRItem() {
        Item item = new Item(random(5), 10, 20);
        GRItem grItem = GRItemFactory.create(item);

        assertThat(grItem).isInstanceOf(RegularGRItem.class);
    }

    @Test
    public void createsConjuredGRItem() {
        Item item = new Item(CONJURED + " " + random(5), 10, 20);
        GRItem grItem = GRItemFactory.create(item);

        assertThat(grItem).isInstanceOf(ConjuredGRItem.class);
    }

    @Test
    public void createsBackstagePassesGRItem() {
        Item item = new Item(BACKSTAGE_PASSES + " " + random(5), 10, 20);
        GRItem grItem = GRItemFactory.create(item);

        assertThat(grItem).isInstanceOf(BackstagePassesGRItem.class);
    }

    @Test
    public void createsAgedBrieGRItem() {
        Item item = new Item(AGED_BRIE, 10, 20);
        GRItem grItem = GRItemFactory.create(item);

        assertThat(grItem).isInstanceOf(AgedBrieGRItem.class);
    }

    @Test
    public void createsSulfurasGRItem() {
        Item item = new Item(SULFURAS, 10, 20);
        GRItem grItem = GRItemFactory.create(item);

        assertThat(grItem).isInstanceOf(SulfurasGRItem.class);
    }
}
