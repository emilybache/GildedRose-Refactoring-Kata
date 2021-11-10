<?php

declare(strict_types=1);

namespace Tests\GildedRose;

use GildedRose\Item;
use GildedRose\StrategyResolverInterface;
use Tests\TestCase;

class BackstagePassesItemTest extends TestCase
{
    /**
     * @group BackstagePasses
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
            'si: 100->99, qty: 10->11' => [
                'items' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_BACKSTAGE_PASSES, 100, 10),
                ],
                'expected' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_BACKSTAGE_PASSES, 99, 11),

                ],
            ],
            'si: 10->9, qty increase +2: 10->12' => [
                'items' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_BACKSTAGE_PASSES, 10, 10),
                ],
                'expected' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_BACKSTAGE_PASSES, 9, 12),

                ],
            ],
            'si: 5->4, qty increase +3: 10->13' => [
                'items' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_BACKSTAGE_PASSES, 5, 10),
                ],
                'expected' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_BACKSTAGE_PASSES, 4, 13),

                ],
            ],
            'si: 10->9, min qty: -100->2' => [
                'items' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_BACKSTAGE_PASSES, 10, -100),
                ],
                'expected' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_BACKSTAGE_PASSES, 9, 2),

                ],
            ],
            'concert is finished, si: 0->-1, min qty: 100->0' => [
                'items' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_BACKSTAGE_PASSES, 0, 100),
                ],
                'expected' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_BACKSTAGE_PASSES, -1, 0),

                ],
            ],
            'si: 1->0, max qty: 50->50' => [
                'items' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_BACKSTAGE_PASSES, 1, 50),
                ],
                'expected' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_BACKSTAGE_PASSES, 0, 50),

                ],
            ],
            'si: 10->9, max qty: 105->50' => [
                'items' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_BACKSTAGE_PASSES, 10, 105),
                ],
                'expected' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_BACKSTAGE_PASSES, 9, 50),

                ],
            ],
        ];
    }

    /**
     * @group BackstagePasses
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
            'si: 100->97, qty: 10->13' => [
                'items' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_BACKSTAGE_PASSES, 100, 10),
                ],
                'expected' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_BACKSTAGE_PASSES, 97, 13),

                ],
            ],
            'si: 10->7, qty increase +2: 10->16' => [
                'items' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_BACKSTAGE_PASSES, 10, 10),
                ],
                'expected' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_BACKSTAGE_PASSES, 7, 16),

                ],
            ],
            'si: 5->2, qty increase +3: 10->19' => [
                'items' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_BACKSTAGE_PASSES, 5, 10),
                ],
                'expected' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_BACKSTAGE_PASSES, 2, 19),

                ],
            ],
            'si: 7->4, qty increase +2 and +3: 10->18' => [
                'items' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_BACKSTAGE_PASSES, 7, 10),
                ],
                'expected' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_BACKSTAGE_PASSES, 4, 18),

                ],
            ],
            'si: 10->7, min qty: -100->6' => [
                'items' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_BACKSTAGE_PASSES, 10, -100),
                ],
                'expected' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_BACKSTAGE_PASSES, 7, 6),

                ],
            ],
            'concert is finished, si: 2->-1, min qty: 100->0' => [
                'items' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_BACKSTAGE_PASSES, 2, 100),
                ],
                'expected' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_BACKSTAGE_PASSES, -1, 0),

                ],
            ],
            'si: 3->0, max qty: 50->50' => [
                'items' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_BACKSTAGE_PASSES, 3, 50),
                ],
                'expected' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_BACKSTAGE_PASSES, 0, 50),

                ],
            ],
            'si: 10->7, max qty: 105->50' => [
                'items' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_BACKSTAGE_PASSES, 10, 105),
                ],
                'expected' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_BACKSTAGE_PASSES, 7, 50),

                ],
            ],
        ];
    }
}
