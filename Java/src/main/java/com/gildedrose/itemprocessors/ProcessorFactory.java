package com.gildedrose.itemprocessors;

import com.gildedrose.itemsorts.QualityItem;
import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

public class ProcessorFactory {
    static final String EXCEPTION_MESSAGE_TEMPLATE = "Item processor for the item of type %s is not found. Make sure that the factory" +
            "is configured correctly";
    private final Map<Class<? extends QualityItem>, QualityItemProcessor<? extends QualityItem>> qualityItemsProcessors = new HashMap<>();


    public ProcessorFactory(List<ProcessorFactoryItem<?>> qualityItemProcessors) {
        qualityItemProcessors.forEach(processorFactoryItem -> qualityItemsProcessors.put(processorFactoryItem.getItemType(),processorFactoryItem.getItemProcessor()));
    }

    @Getter
    @AllArgsConstructor
    public static class ProcessorFactoryItem<T extends QualityItem> {
        private final Class<T> itemType;
        private final QualityItemProcessor<T> itemProcessor;
    }

    public QualityItemProcessor<QualityItem> get(QualityItem qualityItem) {
        Class<? extends QualityItem> aClass = qualityItem.getClass();
        QualityItemProcessor<? extends QualityItem> qualityItemProcessor = Optional.ofNullable(qualityItemsProcessors.get(aClass)).orElseThrow(
                () -> new IllegalArgumentException(String.format(EXCEPTION_MESSAGE_TEMPLATE, aClass)));
        @SuppressWarnings("unchecked") // unnecessary
        QualityItemProcessor<QualityItem> qualityItemProcessorCasted = (QualityItemProcessor<QualityItem>) qualityItemProcessor;
        return qualityItemProcessorCasted;
    }
}
