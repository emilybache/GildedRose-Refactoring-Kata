package com.gildedrose;

public class ItemQualityIsNegativeException extends RuntimeException {

    public ItemQualityIsNegativeException(String name) {
        super("Item '" + name + "' has a negative quality value");
    }
}
