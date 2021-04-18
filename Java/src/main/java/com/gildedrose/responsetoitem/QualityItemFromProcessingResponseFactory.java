package com.gildedrose.responsetoitem;

import com.gildedrose.itemsorts.QualityItem;
import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

public class QualityItemFromProcessingResponseFactory {
    static final String EXCEPTION_MESSAGE_TEMPLATE = "QualityItemFromProcessingResponse for the item of type %s is not found. Make sure that the factory" +
            "is configured correctly";

    Map<Class<? extends QualityItem>, QualityItemFromProcessingResponse<? extends QualityItem>> qualityItemFromProcessingResponseMap = new HashMap<>();

    public <T extends QualityItem> QualityItemFromProcessingResponse<T> get(T qualityItem) {
        Class<? extends QualityItem> aClass = qualityItem.getClass();
        QualityItemFromProcessingResponse<? extends QualityItem> qualityItemProcessor = Optional.ofNullable(qualityItemFromProcessingResponseMap.get(aClass))
                .orElseThrow(
                        () -> new IllegalArgumentException(String.format(EXCEPTION_MESSAGE_TEMPLATE, aClass)));
        @SuppressWarnings("unchecked")
        QualityItemFromProcessingResponse<T> qualityItemFromProcessingResponse =
                (QualityItemFromProcessingResponse<T>) qualityItemProcessor;
        return qualityItemFromProcessingResponse;
    }


    @Getter
    @AllArgsConstructor
    public static class QualityItemFromProcessingResponseFactoryItem<T extends QualityItem> {
        private final Class<T> itemType;
        private final QualityItemFromProcessingResponse<T> itemProcessor;
    }

    public QualityItemFromProcessingResponseFactory(List<QualityItemFromProcessingResponseFactoryItem<?>> qualityItemFromProcessingResponseFactoryItems) {
        qualityItemFromProcessingResponseFactoryItems.forEach(qualityItemFromProcessingResponseFactoryItem ->
                qualityItemFromProcessingResponseMap.put(
                        qualityItemFromProcessingResponseFactoryItem.getItemType(),
                        qualityItemFromProcessingResponseFactoryItem.getItemProcessor())
        );
    }
}
