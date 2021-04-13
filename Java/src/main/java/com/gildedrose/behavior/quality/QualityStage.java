package com.gildedrose.behavior.quality;

public class QualityStage {

    private final int sellInCutoff;
    private final int qualityIncrease;

    private QualityStage(int sellInCutoff, int qualityIncrease) {
        this.sellInCutoff = sellInCutoff;
        this.qualityIncrease = qualityIncrease;
    }

    public static QualityStage of(int sellInCutoff, int qualityIncrease) {
        return new QualityStage(sellInCutoff, qualityIncrease);
    }

    public int getSellInCutoff() {
        return sellInCutoff;
    }

    public int getQualityIncrease() {
        return qualityIncrease;
    }
}
