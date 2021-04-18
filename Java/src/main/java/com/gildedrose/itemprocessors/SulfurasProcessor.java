package com.gildedrose.itemprocessors;


import com.gildedrose.itemsorts.Sulfuras;

public class SulfurasProcessor implements QualityItemProcessor<Sulfuras> {
    @Override
    public ProcessingResponse<Sulfuras> processItem(ProcessingRequest<Sulfuras> processingRequest) {
        return new ProcessingResponse<>(processingRequest.getSellIn(), processingRequest.getQuality());
    }
}
