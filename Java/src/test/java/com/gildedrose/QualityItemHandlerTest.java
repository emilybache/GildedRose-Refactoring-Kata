package com.gildedrose;

import com.gildedrose.responsetoitem.QualityItemFromProcessingResponse;
import com.gildedrose.responsetoitem.QualityItemFromProcessingResponseFactory;
import com.gildedrose.itemprocessors.ProcessorFactory;
import com.gildedrose.itemprocessors.QualityItemProcessor;
import com.gildedrose.itemprocessors.QualityItemProcessor.ProcessingRequest;
import com.gildedrose.itemtorequest.ProcessingRequestFromQualityItem;
import com.gildedrose.itemprocessors.QualityItemProcessor.ProcessingResponse;
import com.gildedrose.itemsorts.NormalItem;
import com.gildedrose.itemsorts.QualityItem;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

import static org.junit.jupiter.api.Assertions.assertEquals;

class QualityItemHandlerTest {

    /**
     * Tests the chain of calls in the {@link QualityItemHandler#processOneItem(QualityItem)} method.
     */
    @Test
    void updateQualityForOneItemTest() {
        ProcessorFactory processorFactoryMock = Mockito.mock(ProcessorFactory.class);
        ProcessingRequestFromQualityItem processingRequestFromQualityItemMock = Mockito.mock(ProcessingRequestFromQualityItem.class);
        QualityItemFromProcessingResponseFactory qualityItemFromProcessingResponseFactoryMock = Mockito.mock(QualityItemFromProcessingResponseFactory.class);


        QualityItemHandler qualityItemHandler = new QualityItemHandler(processorFactoryMock, processingRequestFromQualityItemMock, qualityItemFromProcessingResponseFactoryMock);
        QualityItem qualityItem = new NormalItem("name", 1, 1);
        QualityItem qualityItemResult = new NormalItem("result", 1, 1);

        ProcessingRequest<QualityItem> expectedProcessingRequest = new ProcessingRequest<>(1, 2);
        Mockito.when(processingRequestFromQualityItemMock.get(qualityItem)).thenReturn(expectedProcessingRequest);

        @SuppressWarnings("unchecked")
        QualityItemProcessor<QualityItem> itemProcessorMock = (QualityItemProcessor<QualityItem>) Mockito.mock(QualityItemProcessor.class);
        Mockito.when(processorFactoryMock.get(qualityItem)).thenReturn(itemProcessorMock);


        ProcessingResponse<QualityItem> expectedProcessingResponse = new ProcessingResponse<>(1, 2);
        Mockito.when(itemProcessorMock.processItem(expectedProcessingRequest)).thenReturn(expectedProcessingResponse);


        @SuppressWarnings("unchecked")
        QualityItemFromProcessingResponse<QualityItem> qualityItemQualityItemFromProcessingResponse =
                (QualityItemFromProcessingResponse<QualityItem>) Mockito.mock(QualityItemFromProcessingResponse.class);
        Mockito.when(qualityItemFromProcessingResponseFactoryMock.get(qualityItem)).thenReturn(qualityItemQualityItemFromProcessingResponse);

        Mockito.when(qualityItemQualityItemFromProcessingResponse.toQualityItem(qualityItem, expectedProcessingResponse))
                .thenReturn(qualityItemResult);

        assertEquals(qualityItemResult, qualityItemHandler.processOneItem(qualityItem));
    }
}