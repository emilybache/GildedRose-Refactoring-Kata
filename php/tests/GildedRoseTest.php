<?php

namespace Tests;

use GildedRose\GildedRose;
use GildedRose\Item;
use PHPUnit\Framework\TestCase;

class GildedRoseTest extends TestCase
{
    public function testCommonItemDailyChange(): void
    {
        $items = [new Item('+5 Dexterity Vest', 31, 50)];
        $gildedRose = new GildedRose($items);
        $gildedRose->updateQuality();
        self::assertSame('+5 Dexterity Vest', $items[0]->name);
        self::assertSame(30, $items[0]->sell_in);
        self::assertSame(49, $items[0]->quality);
    }

    public function testQualityDropsTwiceAfterZeroDate(): void
    {
        $items = [new Item('randomItem', -1, 3)];
        $gildedRose = new GildedRose($items);
        $gildedRose->updateQuality();
        self::assertSame(1, $items[0]->quality);
        self::assertSame(-2, $items[0]->sell_in);
    }

    public function testQualityNeverMoreThan50(): void
    {
        $items = [new Item('Aged Brie', -1, 49)];
        $gildedRose = new GildedRose($items);
        $gildedRose->updateQuality();
        self::assertSame(50, $items[0]->quality);
        self::assertSame(-2, $items[0]->sell_in);
    }

    /** @dataProvider qualityNeverNegativeScenario */
    public function testQualityNeverNegative(array $qualityNeverNegativeScenario): void
    {
        $items = [new Item('foo', $qualityNeverNegativeScenario['data']['sell_in'], $qualityNeverNegativeScenario['data']['quality'])];
        $gildedRose = new GildedRose($items);
        $gildedRose->updateQuality();
        self::assertSame($qualityNeverNegativeScenario['expect']['quality'], $items[0]->quality);
        self::assertSame($qualityNeverNegativeScenario['expect']['sell_in'], $items[0]->sell_in);
    }

    public function qualityNeverNegativeScenario(): array
    {
        return [
            [[
                'data' => ['sell_in' => 1, 'quality' => 0],
                'expect' => ['sell_in' => 0, 'quality' => 0],
            ]],
            [[
                'data' => ['sell_in' => -1, 'quality' => 1],
                'expect' => ['sell_in' => -2, 'quality' => 0],
            ]]
        ];
    }
    /** end of QualityNeverNegativeScenario */

    /** @dataProvider agedBrieScenario */
    public function testAgedBrieQualityIncreases(array $agedBrieScenario): void
    {
        $items = [new Item('Aged Brie', $agedBrieScenario['data']['sell_in'], $agedBrieScenario['data']['quality'])];
        $gildedRose = new GildedRose($items);
        $gildedRose->updateQuality();
        self::assertSame($agedBrieScenario['expect']['quality'], $items[0]->quality);
        self::assertSame($agedBrieScenario['expect']['sell_in'], $items[0]->sell_in);
    }

    public function agedBrieScenario(): array
    {
        return [
            [[
                'data' => ['sell_in' => 11, 'quality' => 49],
                'expect' => ['sell_in' => 10, 'quality' => 50],
            ]],
            [[
                'data' => ['sell_in' => 0, 'quality' => 47],
                'expect' => ['sell_in' => -1, 'quality' => 49],
            ]]
        ];
    }
    /** end of agedBrieScenario */

    /** @dataProvider sulfurasScenario */
    public function testSulfurasQualityAndSellinStays(array $sulfurasScenario): void
    {
        $items = [new Item('Sulfuras, Hand of Ragnaros', $sulfurasScenario['data']['sell_in'], $sulfurasScenario['data']['quality'])];
        $gildedRose = new GildedRose($items);
        $gildedRose->updateQuality();
        self::assertSame($sulfurasScenario['expect']['quality'], $items[0]->quality);
        self::assertSame($sulfurasScenario['expect']['sell_in'], $items[0]->sell_in);
    }

    public function sulfurasScenario(): array
    {
        return [
            [[
                'data' => ['sell_in' => 31, 'quality' => 80],
                'expect' => ['sell_in' => 31, 'quality' => 80],
            ]],
            [[
                'data' => ['sell_in' => 0, 'quality' => 80],
                'expect' => ['sell_in' => 0, 'quality' => 80],
            ]],
            [[
                'data' => ['sell_in' => -1, 'quality' => 80],
                'expect' => ['sell_in' => -1, 'quality' => 80],
            ]]
        ];
    }
    /** end of sulfurasScenario */

    /** @dataProvider backstagePassScenario */
    public function testBackstagePassQuality(array $backstagePassScenario): void
    {
        $items = [new Item('Backstage passes to a TAFKAL80ETC concert', $backstagePassScenario['data']['sell_in'], $backstagePassScenario['data']['quality'])];
        $gildedRose = new GildedRose($items);
        $gildedRose->updateQuality();
        self::assertSame($backstagePassScenario['expect']['quality'], $items[0]->quality);
        self::assertSame($backstagePassScenario['expect']['sell_in'], $items[0]->sell_in);
    }

    public function backstagePassScenario(): array
    {
        return [
            [[
                'data' => ['sell_in' => 31, 'quality' => 43],
                'expect' => ['sell_in' => 30, 'quality' => 44],
            ]],
            [[
                'data' => ['sell_in' => 10, 'quality' => 44],
                'expect' => ['sell_in' => 9, 'quality' => 46],
            ]],
            [[
                'data' => ['sell_in' => 1, 'quality' => 46],
                'expect' => ['sell_in' => 0, 'quality' => 49],
            ]],
            [[
                'data' => ['sell_in' => 0, 'quality' => 49],
                'expect' => ['sell_in' => -1, 'quality' => 0],
            ]]
        ];
    }
    /** end of backstageScenario */

    /** @dataProvider conjuredItemsScenario */
    public function testConjuredItems(array $conjuredItemsScenario): void
    {
        $items = [new Item('Conjured Mana Cake', $conjuredItemsScenario['data']['sell_in'], $conjuredItemsScenario['data']['quality'])];
        $gildedRose = new GildedRose($items);
        $gildedRose->updateQuality();
        self::assertSame($conjuredItemsScenario['expect']['quality'], $items[0]->quality);
        self::assertSame($conjuredItemsScenario['expect']['sell_in'], $items[0]->sell_in);
    }

    public function conjuredItemsScenario(): array
    {
        return [
            [[
                'data' => ['sell_in' => 31, 'quality' => 50],
                'expect' => ['sell_in' => 30, 'quality' => 48],
            ]],
            [[
                'data' => ['sell_in' => 0, 'quality' => 48],
                'expect' => ['sell_in' => -1, 'quality' => 44],
            ]]
        ];
    }
    /** end of conjuredItemsScenario */
}