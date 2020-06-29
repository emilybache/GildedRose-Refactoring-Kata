package com.gildedrose;

public class ItemQualityExceedsMaxValueException extends RuntimeException {

    public ItemQualityExceedsMaxValueException(String name) {
        super("Item '" + name + "' exceeds max value for quality ");
    }
}
