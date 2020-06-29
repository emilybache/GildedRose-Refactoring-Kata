package com.gildedrose.item;

import com.gildedrose.Item;
import com.gildedrose.item.GRItem;
import com.gildedrose.item.GRItemFactory;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import static com.gildedrose.item.GRItemFactory.SULFURAS;
import static org.assertj.core.api.Assertions.assertThat;

public class SulfurasGRItemUpdateQualityTest {

    @ParameterizedTest(name = "with quality {0}")
    @ValueSource(ints = {0, 1, 20, 50})
    public void doesNothing(int quality) {
        Item item = new Item(SULFURAS, 10, quality);
        GRItem grItem = GRItemFactory.create(item);
        grItem.updateQuality();

        assertThat(item.sellIn).isEqualTo(10);
        assertThat(item.quality).isEqualTo(quality);
    }
}

