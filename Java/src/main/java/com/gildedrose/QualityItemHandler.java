package com.gildedrose;


import com.gildedrose.itemprocessors.ProcessorFactory;
import com.gildedrose.itemsorts.QualityItem;
import com.gildedrose.itemtorequest.ProcessingRequestFromQualityItem;
import com.gildedrose.responsetoitem.QualityItemFromProcessingResponseFactory;

public class QualityItemHandler {
    private final ProcessorFactory processorFactory;
    private final ProcessingRequestFromQualityItem processingRequestFromQualityItem;
    private final QualityItemFromProcessingResponseFactory qualityItemFromProcessingResponseFactory;

    QualityItemHandler(ProcessorFactory processorFactory, ProcessingRequestFromQualityItem processingRequestFromQualityItem,
                       QualityItemFromProcessingResponseFactory qualityItemFromProcessingResponseFactory) {
        this.processorFactory = processorFactory;
        this.processingRequestFromQualityItem = processingRequestFromQualityItem;
        this.qualityItemFromProcessingResponseFactory = qualityItemFromProcessingResponseFactory;
    }

    QualityItem processOneItem(QualityItem qualityItem) {
        return qualityItemFromProcessingResponseFactory.get(qualityItem).toQualityItem(
                qualityItem,
                processorFactory.get(qualityItem).
                        processItem(processingRequestFromQualityItem.get(qualityItem))
        );
    }
}
