package com.gildedrose;

import com.gildedrose.responsetoitem.AgedBrieFromProcessingResponse;
import com.gildedrose.responsetoitem.BackstagePassFromProcessingResponse;
import com.gildedrose.responsetoitem.ConjuredItemFromProcessingResponse;
import com.gildedrose.responsetoitem.NormalItemFromProcessingResponse;
import com.gildedrose.responsetoitem.QualityItemFromProcessingResponseFactory;
import com.gildedrose.responsetoitem.QualityItemFromProcessingResponseFactory.QualityItemFromProcessingResponseFactoryItem;
import com.gildedrose.responsetoitem.SulfurasFromProcessingResponse;
import com.gildedrose.itemprocessors.AgedBrieProcessor;
import com.gildedrose.itemprocessors.BackstagePassProcessor;
import com.gildedrose.itemprocessors.ConjuredItemProcessor;
import com.gildedrose.itemprocessors.NormalItemProcessor;
import com.gildedrose.itemprocessors.ProcessorFactory;
import com.gildedrose.itemprocessors.ProcessorFactory.ProcessorFactoryItem;
import com.gildedrose.itemtorequest.ProcessingRequestFromQualityItem;
import com.gildedrose.itemprocessors.SulfurasProcessor;
import com.gildedrose.itemsorts.AgedBrie;
import com.gildedrose.itemsorts.BackstagePass;
import com.gildedrose.itemsorts.ConjuredItem;
import com.gildedrose.itemsorts.NormalItem;
import com.gildedrose.itemsorts.QualityItem;
import com.gildedrose.itemsorts.Sulfuras;

import java.util.Arrays;
import java.util.List;

public class TexttestFixture {
    public static void main(String[] args) {
        System.out.println("OMGHAI!");

        List<QualityItem> items = Arrays.asList(
                new NormalItem("+5 Dexterity Vest", 10, 20), //
                new AgedBrie("Aged Brie", 2, 0), //
                new NormalItem("Elixir of the Mongoose", 5, 7), //
                new Sulfuras("Sulfuras, Hand of Ragnaros", 0, 80), //
                new Sulfuras("Sulfuras, Hand of Ragnaros", -1, 80),
                new BackstagePass("Backstage passes to a TAFKAL80ETC concert", 15, 20),
                new BackstagePass("Backstage passes to a TAFKAL80ETC concert", 10, 49),
                new BackstagePass("Backstage passes to a TAFKAL80ETC concert", 5, 49),
                // this conjured item does not work properly yet
                new ConjuredItem("Conjured Mana Cake", 3, 6)
        );

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

        int days = 2;
        if (args.length > 0) {
            days = Integer.parseInt(args[0]) + 1;
        }

        for (int i = 0; i < days; i++) {
            System.out.println("-------- day " + i + " --------");
            System.out.println("name, sellIn, quality");
            for (QualityItem item : items) {
                System.out.println(item);
            }
            System.out.println();
            app.updateQuality();
        }
    }

}
