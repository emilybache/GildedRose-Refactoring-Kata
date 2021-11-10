<?php

declare(strict_types=1);

namespace Tests;

use GildedRose\Item;
use GildedRose\StrategyResolverInterface;

class GildedRoseTest extends TestCase
{
    /**
     * @group GildedRose
     */
    public function testFooItemNameIsTheSameAfterUpdatedQuality(): void
    {
        $items = [new Item('foo', 0, 0)];
        $gildedRose = $this->initializeGlidedRose($items);

        $gildedRose->updateQuality();

        $this->assertSame('foo', $items[0]->name);
    }

    /**
     * @group GildedRose
     */
    public function testDifferentMultiItemsDataIsCorrectAfterUpdatedQuality(): void
    {
        $items = [
            new Item('another foo', 1, 10),
            new Item(StrategyResolverInterface::ITEM_NAME_AGED_BRIE, 0, 10),
            new Item(StrategyResolverInterface::ITEM_NAME_SULFURAS, 777, 777),
            new Item(StrategyResolverInterface::ITEM_NAME_BACKSTAGE_PASSES, 5, 10),
            new Item(StrategyResolverInterface::ITEM_NAME_CONJURED, 1, 10),
        ];
        $gildedRose = $this->initializeGlidedRose($items);

        $gildedRose->updateQuality();

        $this->assertSameItem(new Item('another foo', 0, 9), $items[0]);
        $this->assertSameItem(new Item(StrategyResolverInterface::ITEM_NAME_AGED_BRIE, -1, 12), $items[1]);
        $this->assertSameItem(new Item(StrategyResolverInterface::ITEM_NAME_SULFURAS, 777, 777), $items[2]);
        $this->assertSameItem(new Item(StrategyResolverInterface::ITEM_NAME_BACKSTAGE_PASSES, 4, 13), $items[3]);
        $this->assertSameItem(new Item(StrategyResolverInterface::ITEM_NAME_CONJURED, 0, 8), $items[4]);
    }

    /**
     * @group GildedRose
     */
    public function testSameMultiItemsDataIsCorrectAfterUpdatedQuality(): void
    {
        $items = [
            new Item(StrategyResolverInterface::ITEM_NAME_BACKSTAGE_PASSES, 0, 10),
            new Item(StrategyResolverInterface::ITEM_NAME_BACKSTAGE_PASSES, 1, 10),
            new Item(StrategyResolverInterface::ITEM_NAME_BACKSTAGE_PASSES, 5, 10),
            new Item(StrategyResolverInterface::ITEM_NAME_BACKSTAGE_PASSES, 10, 10),
            new Item(StrategyResolverInterface::ITEM_NAME_BACKSTAGE_PASSES, 100, 10),
        ];
        $gildedRose = $this->initializeGlidedRose($items);

        $gildedRose->updateQuality();

        $this->assertSameItem(new Item(StrategyResolverInterface::ITEM_NAME_BACKSTAGE_PASSES, -1, 0), $items[0]);
        $this->assertSameItem(new Item(StrategyResolverInterface::ITEM_NAME_BACKSTAGE_PASSES, 0, 13), $items[1]);
        $this->assertSameItem(new Item(StrategyResolverInterface::ITEM_NAME_BACKSTAGE_PASSES, 4, 13), $items[2]);
        $this->assertSameItem(new Item(StrategyResolverInterface::ITEM_NAME_BACKSTAGE_PASSES, 9, 12), $items[3]);
        $this->assertSameItem(new Item(StrategyResolverInterface::ITEM_NAME_BACKSTAGE_PASSES, 99, 11), $items[4]);
    }

    /**
     * @group GildedRose
     */
    public function testDifferentMultiItemsDataIsCorrectAfterUpdatedQualityThirtyTimes(): void
    {
        $items = [
            new Item('another super foo', 20, 60),
            new Item(StrategyResolverInterface::ITEM_NAME_AGED_BRIE, 20, 0),
            new Item(StrategyResolverInterface::ITEM_NAME_SULFURAS, 777, 777),
            new Item(StrategyResolverInterface::ITEM_NAME_BACKSTAGE_PASSES, 31, 0),
            new Item(StrategyResolverInterface::ITEM_NAME_CONJURED, 28, 777),
        ];
        $gildedRose = $this->initializeGlidedRose($items);

        for ($i = 0; $i < 30; $i++) {
            $gildedRose->updateQuality();
        }

        $this->assertSameItem(new Item('another super foo', -10, 10), $items[0]);
        $this->assertSameItem(new Item(StrategyResolverInterface::ITEM_NAME_AGED_BRIE, -10, 40), $items[1]);
        $this->assertSameItem(new Item(StrategyResolverInterface::ITEM_NAME_SULFURAS, 777, 777), $items[2]);
        $this->assertSameItem(new Item(StrategyResolverInterface::ITEM_NAME_BACKSTAGE_PASSES, 1, 45), $items[3]);
        $this->assertSameItem(new Item(StrategyResolverInterface::ITEM_NAME_CONJURED, -2, 0), $items[4]);
    }
}
