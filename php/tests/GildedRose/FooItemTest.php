<?php

declare(strict_types=1);

namespace Tests\GildedRose;

use GildedRose\Item;
use Tests\TestCase;

class FooItemTest extends TestCase
{
    /**
     * @group FooItem
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
            'si: 0->-1, qty decrease x2: 10->8' => [
                'items' => [
                    'foo' => new Item('foo', 0, 10),
                ],
                'expected' => [
                    'foo' => new Item('foo', -1, 8),

                ],
            ],
            'si: 1->0, qty: 10->9' => [
                'items' => [
                    'foo' => new Item('foo', 1, 10),
                ],
                'expected' => [
                    'foo' => new Item('foo', 0, 9),

                ],
            ],
            'si: 10->9, min qty: 0->0' => [
                'items' => [
                    'foo' => new Item('foo', 10, 0),
                ],
                'expected' => [
                    'foo' => new Item('foo', 9, 0),

                ],
            ],
            'si: -10->-11, qty: 1->0' => [
                'items' => [
                    'foo' => new Item('foo', -10, 1),
                ],
                'expected' => [
                    'foo' => new Item('foo', -11, 0),

                ],
            ],
            'si: 10->9, min qty: -50->0' => [
                'items' => [
                    'foo' => new Item('foo', 10, -50),
                ],
                'expected' => [
                    'foo' => new Item('foo', 9, 0),

                ],
            ],
            'si: 10->9, max qty: 105->49' => [
                'items' => [
                    'foo' => new Item('foo', 10, 105),
                ],
                'expected' => [
                    'foo' => new Item('foo', 9, 49),

                ],
            ],
        ];
    }

    /**
     * @group FooItem
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
            'si: 0->-3, qty decrease x2: 10->4' => [
                'items' => [
                    'foo' => new Item('foo', 0, 10),
                ],
                'expected' => [
                    'foo' => new Item('foo', -3, 4),

                ],
            ],
            'si: 3->0, qty: 10->7' => [
                'items' => [
                    'foo' => new Item('foo', 3, 10),
                ],
                'expected' => [
                    'foo' => new Item('foo', 0, 7),

                ],
            ],
            'si: 1->-2, qty decrease x1 and x2: 10->5' => [
                'items' => [
                    'foo' => new Item('foo', 1, 10),
                ],
                'expected' => [
                    'foo' => new Item('foo', -2, 5),

                ],
            ],
            'si: 10->7, min qty: 0->0' => [
                'items' => [
                    'foo' => new Item('foo', 10, 0),
                ],
                'expected' => [
                    'foo' => new Item('foo', 7, 0),

                ],
            ],
            'si: -10->-13, qty: 1->0' => [
                'items' => [
                    'foo' => new Item('foo', -10, 1),
                ],
                'expected' => [
                    'foo' => new Item('foo', -13, 0),

                ],
            ],
            'si: 10->7, max qty: 105->47' => [
                'items' => [
                    'foo' => new Item('foo', 10, 105),
                ],
                'expected' => [
                    'foo' => new Item('foo', 7, 47),

                ],
            ],
        ];
    }
}
