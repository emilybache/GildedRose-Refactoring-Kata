<?php

declare(strict_types=1);

namespace Tests\GildedRose;

use GildedRose\Item;
use GildedRose\StrategyResolverInterface;
use Tests\TestCase;

class ConjuredItemTest extends TestCase
{
    /**
     * @group ConjuredItem
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
            'si: 0->-1, qty decrease x4: 10->8' => [
                'items' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_CONJURED, 0, 10),
                ],
                'expected' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_CONJURED, -1, 6),

                ],
            ],
            'si: 1->0, qty decrease x2: 10->9' => [
                'items' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_CONJURED, 1, 10),
                ],
                'expected' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_CONJURED, 0, 8),

                ],
            ],
            'si: 10->9, min qty: 0->0' => [
                'items' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_CONJURED, 10, 0),
                ],
                'expected' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_CONJURED, 9, 0),

                ],
            ],
            'si: -10->-11, min qty: 3->0' => [
                'items' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_CONJURED, -10, 3),
                ],
                'expected' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_CONJURED, -11, 0),

                ],
            ],
            'si: 10->9, min qty: -50->0' => [
                'items' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_CONJURED, 10, -50),
                ],
                'expected' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_CONJURED, 9, 0),

                ],
            ],
            'si: 10->9, max qty: 105->48' => [
                'items' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_CONJURED, 10, 105),
                ],
                'expected'
=> [
    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_CONJURED, 9, 48),

],
            ],
        ];
    }

    /**
     * @skip
     * @group ConjuredItem
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
            'si: 0->-3, qty decrease x4: 20->8' => [
                'items' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_CONJURED, 0, 20),
                ],
                'expected' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_CONJURED, -3, 8),

                ],
            ],
            'si: 3->0, qty decrease x2: 10->4' => [
                'items' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_CONJURED, 3, 10),
                ],
                'expected' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_CONJURED, 0, 4),

                ],
            ],
            'si: 1->-2, qty decrease x2 and x4: 20->10' => [
                'items' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_CONJURED, 1, 20),
                ],
                'expected' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_CONJURED, -2, 10),

                ],
            ],
            'si: 10->7, min qty: 0->0' => [
                'items' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_CONJURED, 10, 0),
                ],
                'expected' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_CONJURED, 7, 0),

                ],
            ],
            'si: -10->-13, qty: 11->0' => [
                'items' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_CONJURED, -10, 11),
                ],
                'expected' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_CONJURED, -13, 0),

                ],
            ],
            'si: 10->7, min qty: -50->0' => [
                'items' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_CONJURED, 10, -50),
                ],
                'expected' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_CONJURED, 7, 0),

                ],
            ],
            'si: 10->7, max qty: 105->44' => [
                'items' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_CONJURED, 10, 105),
                ],
                'expected' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_CONJURED, 7, 44),

                ],
            ],
        ];
    }
}
