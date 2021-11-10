<?php

declare(strict_types=1);

namespace Tests\GildedRose;

use GildedRose\Item;
use GildedRose\StrategyResolverInterface;
use Tests\TestCase;

class AgedBrieItemTest extends TestCase
{
    /**
     * @group AgedBrie
     * @dataProvider sellInAndQualityIsCorrectAfterUpdatedQualityDataProvider
     *
     * @param Item[] $items
     * @param Item[] $expectedItems
     */
    public function testSellInAndQualityIsCorrectAfterUpdatedQuality(
        array $items,
        array $expectedItems
    ): void {
        $gildedRose = $this->initializeGlidedRose($items);

        $gildedRose->updateQuality();

        $this->assertSameItems($expectedItems, $items);
    }

    public function sellInAndQualityIsCorrectAfterUpdatedQualityDataProvider(): array
    {
        return [
            'si: 0->-1, qty decrease x2: 10->12' => [
                'items' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_AGED_BRIE, 0, 10),
                ],
                'expected' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_AGED_BRIE, -1, 12),

                ],
            ],
            'si: 1->0, qty: 10->11' => [
                'items' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_AGED_BRIE, 1, 10),
                ],
                'expected' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_AGED_BRIE, 0, 11),

                ],
            ],
            'si: 10->9, min qty: -100->1' => [
                'items' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_AGED_BRIE, 10, -100),
                ],
                'expected' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_AGED_BRIE, 9, 1),

                ],
            ],
            'si: -10->-11, max qty: 49->50' => [
                'items' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_AGED_BRIE, -10, 49),
                ],
                'expected' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_AGED_BRIE, -11, 50),

                ],
            ],
            'si: 1->0, max qty: 50->50' => [
                'items' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_AGED_BRIE, 1, 50),
                ],
                'expected' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_AGED_BRIE, 0, 50),

                ],
            ],
            'si: 10->9, max qty: 105->50' => [
                'items' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_AGED_BRIE, 10, 105),
                ],
                'expected'
=> [
    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_AGED_BRIE, 9, 50),

],
            ],
        ];
    }

    /**
     * @group AgedBrie
     * @dataProvider sellInAndQualityIsCorrectAfterUpdatedQualityThreeTimesDataProvider
     *
     * @param Item[] $items
     * @param Item[] $expectedItems
     */
    public function testSellInAndQualityIsCorrectAfterUpdatedQualityThreeTimes(
        array $items,
        array $expectedItems
    ): void {
        $gildedRose = $this->initializeGlidedRose($items);

        $gildedRose->updateQuality();
        $gildedRose->updateQuality();
        $gildedRose->updateQuality();

        $this->assertSameItems($expectedItems, $items);
    }

    public function sellInAndQualityIsCorrectAfterUpdatedQualityThreeTimesDataProvider(): array
    {
        return [
            'si: 0->-3, qty decrease x2: 10->16' => [
                'items' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_AGED_BRIE, 0, 10),
                ],
                'expected' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_AGED_BRIE, -3, 16),

                ],
            ],
            'si: 1->-2, qty: 10->15' => [
                'items' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_AGED_BRIE, 1, 10),
                ],
                'expected' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_AGED_BRIE, -2, 15),

                ],
            ],
            'si: 10->7, min qty: -100->3' => [
                'items' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_AGED_BRIE, 10, -100),
                ],
                'expected' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_AGED_BRIE, 7, 3),

                ],
            ],
            'si: -10->-13, max qty: 45->50' => [
                'items' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_AGED_BRIE, -10, 45),
                ],
                'expected' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_AGED_BRIE, -13, 50),

                ],
            ],
            'si: 1->-2, max qty: 50->50' => [
                'items' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_AGED_BRIE, 1, 50),
                ],
                'expected' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_AGED_BRIE, -2, 50),

                ],
            ],
            'si: 10->7, max qty: 105->50' => [
                'items' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_AGED_BRIE, 10, 105),
                ],
                'expected'
=> [
    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_AGED_BRIE, 7, 50),

],
            ],
        ];
    }
}
