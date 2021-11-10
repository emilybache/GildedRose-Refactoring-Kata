<?php

declare(strict_types=1);

namespace Tests\GildedRose;

use GildedRose\Item;
use GildedRose\StrategyResolverInterface;
use Tests\TestCase;

class SulfurasItemTest extends TestCase
{
    /**
     * @group Sulfuras
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
            'sell-in const: -1->-1, qty const: 10->10' => [
                'items' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_SULFURAS, -1, 10),
                ],
                'expected' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_SULFURAS, -1, 10),

                ],
            ],
            'sell-in const: 0->0, min qty: -100->-100' => [
                'items' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_SULFURAS, 0, -100),
                ],
                'expected' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_SULFURAS, 0, -100),

                ],
            ],
            'sell-in const: 1->1, max qty: 105->105' => [
                'items' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_SULFURAS, 1, 105),
                ],
                'expected' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_SULFURAS, 1, 105),

                ],
            ],
        ];
    }

    /**
     * @group Sulfuras
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
            'sell-in const: -1->-1, qty const: 10->10' => [
                'items' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_SULFURAS, -1, 10),
                ],
                'expected' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_SULFURAS, -1, 10),

                ],
            ],
            'sell-in const: 0->0, min qty: -100->-100' => [
                'items' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_SULFURAS, 0, -100),
                ],
                'expected' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_SULFURAS, 0, -100),

                ],
            ],
            'sell-in const: 1->1, max qty: 105->105' => [
                'items' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_SULFURAS, 1, 105),
                ],
                'expected' => [
                    'foo' => new Item(StrategyResolverInterface::ITEM_NAME_SULFURAS, 1, 105),

                ],
            ],
        ];
    }
}
