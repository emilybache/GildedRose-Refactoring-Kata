<?php

namespace Tests;

use GildedRose\GildedRose;
use GildedRose\Item;
use PHPUnit\Framework\TestCase;

class GildedRoseTest extends TestCase
{
    public function testFoo(): void
    {
        $items = [new Item('foo', 0, 0)];
        $gildedRose = new GildedRose($items);
        $gildedRose->updateQuality();
        self::assertSame('foo', $items[0]->name);
        self::assertSame(-1, $items[0]->sell_in);

    }

    public function testNotBelowZero(): void
    {
        $items = [new Item('foo', 0, 1)];
        $gildedRose = new GildedRose($items);
        $gildedRose->updateQuality();
        self::assertSame(0, $items[0]->quality);
        self::assertSame(-1, $items[0]->sell_in);
    }

    public function testQualityDropsTwice(): void
    {
        $items = [new Item('foo', 0, 2)];
        $gildedRose = new GildedRose($items);
        $gildedRose->updateQuality();
        self::assertSame(0, $items[0]->quality);
        self::assertSame(-1, $items[0]->sell_in);
    }

    public function testAgedBrieQualityIncreases(): void
    {
        $items = [new Item('Aged Brie', 0, 1)];
        $gildedRose = new GildedRose($items);
        $gildedRose->updateQuality();
        self::assertSame(3, $items[0]->quality);
        self::assertSame(-1, $items[0]->sell_in);
    }

    public function testQualityNotMoreThan50(): void
    {
        $items = [new Item('Aged Brie', -1, 47)];
        $gildedRose = new GildedRose($items);
        $gildedRose->updateQuality();
        self::assertSame(49, $items[0]->quality);
        self::assertSame(-2, $items[0]->sell_in);
    }

    public function testSulfurasQualityAndSellinStays(): void
    {
        $items = [new Item('Sulfuras, Hand of Ragnaros', 1, 47)];
        $gildedRose = new GildedRose($items);
        $gildedRose->updateQuality();
        self::assertSame(47, $items[0]->quality);
        self::assertSame(1, $items[0]->sell_in);
    }

    /** @dataProvider BackstageScenario */
    public function testBackstageQuality(array $scenario): void
    {
        $items = [new Item('Backstage passes to a TAFKAL80ETC concert', $scenario['data']['sell_in'], $scenario['data']['quality'])];
        $gildedRose = new GildedRose($items);
        $gildedRose->updateQuality();
        self::assertSame($scenario['expect']['quality'], $items[0]->quality);
        self::assertSame($scenario['expect']['sell_in'], $items[0]->sell_in);
    }

    public function BackstageScenario(): array
    {
        return [
            [[
                'data' => ['sell_in' => 1, 'quality' => 47],
                'expect' => ['sell_in' => 0, 'quality' => 50],
            ]],
            [[
                'data' => ['sell_in' => 5, 'quality' => 50],
                'expect' => ['sell_in' => 4, 'quality' => 50],
            ]],
            [[
                'data' => ['sell_in' => 9, 'quality' => 40],
                'expect' => ['sell_in' => 8, 'quality' => 42],
            ]]
        ];
    }
}