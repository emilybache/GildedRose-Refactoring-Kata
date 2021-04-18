package com.gildedrose.responsetoitem;

import com.gildedrose.responsetoitem.QualityItemFromProcessingResponseFactory.QualityItemFromProcessingResponseFactoryItem;
import com.gildedrose.itemsorts.NormalItem;
import com.gildedrose.itemsorts.QualityItem;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

import java.util.Collections;

import static com.gildedrose.responsetoitem.QualityItemFromProcessingResponseFactory.EXCEPTION_MESSAGE_TEMPLATE;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class QualityItemFromProcessingResponseFactoryTest {
    @Test
    public void testFactoryCannotHandleAnItem() {
        QualityItemFromProcessingResponseFactory factory = new QualityItemFromProcessingResponseFactory(Collections.emptyList());
        QualityItem itemThatCannotBeProcessed = Mockito.mock(QualityItem.class);
        boolean exceptionWasThrown = false;
        try {
            factory.get(itemThatCannotBeProcessed);
        } catch (IllegalArgumentException e) {
            exceptionWasThrown = true;
            assertEquals(String.format(EXCEPTION_MESSAGE_TEMPLATE, itemThatCannotBeProcessed.getClass()), e.getMessage());
        }
        assertTrue(exceptionWasThrown);
    }

    @Test
    public void testFactoryCanHandleAnItem() {
        QualityItemFromProcessingResponse<NormalItem> normalItemProcessor = new NormalItemFromProcessingResponse();
        NormalItem normalItem = new NormalItem("name", 1, 2);
        QualityItemFromProcessingResponseFactory qualityItemFromProcessingResponseFactory =
                new QualityItemFromProcessingResponseFactory(Collections.singletonList(
                        new QualityItemFromProcessingResponseFactoryItem<>(NormalItem.class, normalItemProcessor)
                ));
        assertEquals(normalItemProcessor, qualityItemFromProcessingResponseFactory.get(normalItem));
    }
}