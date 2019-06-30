package com.gildedrose.rules;

public class Result {
    final int quality;
    final boolean isFinalValue;

    public Result(int quality, boolean isFinalValue) {
        this.quality = quality;
        this.isFinalValue = isFinalValue;
    }

    public int getQuality() {
        return quality;
    }

    public boolean isFinalValue() {
        return isFinalValue;
    }
}
