package com.gildedrose;

import org.junit.jupiter.api.Test;

import static org.apache.commons.lang3.RandomStringUtils.random;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

class GRItemValidateItemTest {

    private static final String SULFURAS = "Sulfuras, Hand of Ragnaros";

    @Test
    public void throwsExceptionWhenItemValueIsNegative() {
        Item item = new Item(random(5), 10, -1);

        assertThatThrownBy(() -> GRItem.validateItem(item)).isInstanceOf(ItemQualityIsNegativeException.class);
    }

    @Test
    public void throwsExceptionWhenItemValueExceedsMaxValue() {
        Item item = new Item(random(5), 10, 51);

        assertThatThrownBy(() -> GRItem.validateItem(item)).isInstanceOf(ItemQualityExceedsMaxValueException.class);
    }


    @Test
    public void doesNotThrowExceptionWhenItemValueExceedsMaxValueAndItemIsSulfuras() {
        Item item = new Item(SULFURAS, 10, 51);

        GRItem.validateItem(item);
    }
}