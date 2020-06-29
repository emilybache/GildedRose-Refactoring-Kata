package com.gildedrose.item;

import com.gildedrose.Item;
import org.junit.jupiter.api.Test;

import static org.apache.commons.lang3.RandomStringUtils.random;
import static org.apache.commons.lang3.RandomUtils.nextInt;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

class GRItemValidateItemTest {

    private static final String SULFURAS = "Sulfuras, Hand of Ragnaros";

    @Test
    public void throwsExceptionWhenItemValueIsNegative() {
        Item item = new Item(random(5), nextInt(0, 10), -1);

        assertThatThrownBy(() -> GRItemFactory.create(item)).isInstanceOf(ItemQualityIsNegativeException.class);
    }

    @Test
    public void throwsExceptionWhenItemValueExceedsMaxValue() {
        Item item = new Item(random(5), nextInt(0, 10), 51);

        assertThatThrownBy(() -> GRItemFactory.create(item)).isInstanceOf(ItemQualityExceedsMaxValueException.class);
    }


    @Test
    public void doesNotThrowExceptionWhenItemValueExceedsMaxValueAndItemIsSulfuras() {
        Item item = new Item(SULFURAS, nextInt(0, 10), 51);

        GRItemFactory.create(item);
    }
}