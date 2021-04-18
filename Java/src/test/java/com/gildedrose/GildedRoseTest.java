package com.gildedrose;

import com.gildedrose.itemprocessors.AgedBrieProcessor;
import com.gildedrose.itemprocessors.BackstagePassProcessor;
import com.gildedrose.itemprocessors.ConjuredItemProcessor;
import com.gildedrose.itemprocessors.NormalItemProcessor;
import com.gildedrose.itemprocessors.ProcessorFactory;
import com.gildedrose.itemprocessors.ProcessorFactory.ProcessorFactoryItem;
import com.gildedrose.itemprocessors.SulfurasProcessor;
import com.gildedrose.itemsorts.AgedBrie;
import com.gildedrose.itemsorts.BackstagePass;
import com.gildedrose.itemsorts.ConjuredItem;
import com.gildedrose.itemsorts.NormalItem;
import com.gildedrose.itemsorts.QualityItem;
import com.gildedrose.itemsorts.Sulfuras;
import com.gildedrose.itemtorequest.ProcessingRequestFromQualityItem;
import com.gildedrose.responsetoitem.AgedBrieFromProcessingResponse;
import com.gildedrose.responsetoitem.BackstagePassFromProcessingResponse;
import com.gildedrose.responsetoitem.ConjuredItemFromProcessingResponse;
import com.gildedrose.responsetoitem.NormalItemFromProcessingResponse;
import com.gildedrose.responsetoitem.QualityItemFromProcessingResponseFactory;
import com.gildedrose.responsetoitem.QualityItemFromProcessingResponseFactory.QualityItemFromProcessingResponseFactoryItem;
import com.gildedrose.responsetoitem.SulfurasFromProcessingResponse;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class GildedRoseTest {

    @Test
    void foo() {
        List<QualityItem> items = new ArrayList<>();
        items.add(new NormalItem("foo", 0, 0));

        List<ProcessorFactoryItem<?>> processorFactoryItems = Arrays.asList(
                new ProcessorFactoryItem<>(NormalItem.class, new NormalItemProcessor()),
                new ProcessorFactoryItem<>(AgedBrie.class, new AgedBrieProcessor()),
                new ProcessorFactoryItem<>(Sulfuras.class, new SulfurasProcessor()),
                new ProcessorFactoryItem<>(BackstagePass.class, new BackstagePassProcessor()),
                new ProcessorFactoryItem<>(ConjuredItem.class, new ConjuredItemProcessor()));
        ProcessorFactory processorFactory = new ProcessorFactory(processorFactoryItems);

        List<QualityItemFromProcessingResponseFactoryItem<?>> qualityItemFromProcessingResponseFactoryItems = Arrays.asList(
                new QualityItemFromProcessingResponseFactoryItem<>(NormalItem.class, new NormalItemFromProcessingResponse()),
                new QualityItemFromProcessingResponseFactoryItem<>(AgedBrie.class, new AgedBrieFromProcessingResponse()),
                new QualityItemFromProcessingResponseFactoryItem<>(Sulfuras.class, new SulfurasFromProcessingResponse()),
                new QualityItemFromProcessingResponseFactoryItem<>(BackstagePass.class, new BackstagePassFromProcessingResponse()),
                new QualityItemFromProcessingResponseFactoryItem<>(ConjuredItem.class, new ConjuredItemFromProcessingResponse()));
        QualityItemFromProcessingResponseFactory qualityItemFromProcessingResponseFactory =
                new QualityItemFromProcessingResponseFactory(qualityItemFromProcessingResponseFactoryItems);

        ProcessingRequestFromQualityItem processingRequestFromQualityItem = new ProcessingRequestFromQualityItem();

        QualityItemHandler qualityItemHandler = new QualityItemHandler(processorFactory, processingRequestFromQualityItem,
                qualityItemFromProcessingResponseFactory);

        GildedRose app = new GildedRose(items, qualityItemHandler);
        app.updateQuality();
        assertEquals("foo", app.getItems().get(0).getName());
    }

    /**
     * Check that {@link GildedRose#updateQuality()} will call {@link QualityItemHandler#processOneItem(QualityItem)}
     * for every element in the GildedRose
     */
    @Test
    void updateQualityTest() {
        List<QualityItem> calledForItems = new ArrayList<>();
        List<QualityItem> inputItems = new ArrayList<>();
        NormalItem expectedItem1 = new NormalItem("name1", 1, 2);
        NormalItem expectedItem2 = new NormalItem("name2", 1, 2);
        inputItems.add(expectedItem1);
        inputItems.add(expectedItem2);

        QualityItemHandler qualityItemHandler = new QualityItemHandler(null, null, null) {
            @Override
            QualityItem processOneItem(QualityItem qualityItem) {
                calledForItems.add(qualityItem);
                return qualityItem;
            }
        };
        GildedRose gildedRose = new GildedRose(inputItems, qualityItemHandler);

        gildedRose.updateQuality();
        assertTrue(calledForItems.containsAll(inputItems));
        calledForItems.removeAll(inputItems);
        assertTrue(calledForItems.isEmpty());
    }
}
