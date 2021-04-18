package com.gildedrose.itemprocessors;

import com.gildedrose.itemprocessors.ProcessorFactory.ProcessorFactoryItem;
import com.gildedrose.itemsorts.NormalItem;
import com.gildedrose.itemsorts.QualityItem;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;


import java.util.Collections;

import static com.gildedrose.itemprocessors.ProcessorFactory.EXCEPTION_MESSAGE_TEMPLATE;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ProcessorFactoryTest {

    @Test
    public void testFactoryCannotHandleItem() {
        ProcessorFactory processorFactory = new ProcessorFactory(Collections.emptyList());
        QualityItem itemThatCannotBeProcessed = Mockito.mock(QualityItem.class);
        boolean exceptionWasThrown = false;
        try {
            processorFactory.get(itemThatCannotBeProcessed);
        } catch (IllegalArgumentException e) {
            exceptionWasThrown = true;
            assertEquals(String.format(EXCEPTION_MESSAGE_TEMPLATE, itemThatCannotBeProcessed.getClass()), e.getMessage());
        }
        assertTrue(exceptionWasThrown);
    }

    @Test
    public void testFactoryCanHandleItem() {
        QualityItemProcessor<NormalItem> normalItemProcessor = new NormalItemProcessor();
        NormalItem normalItem = new NormalItem("name", 1, 2);
        ProcessorFactory processorFactory = new ProcessorFactory(Collections.singletonList(new ProcessorFactoryItem<>(NormalItem.class, normalItemProcessor)));
        assertEquals(normalItemProcessor, processorFactory.get(normalItem));
    }
}