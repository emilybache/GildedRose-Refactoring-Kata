<?php

declare(strict_types=1);

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
        $this->assertSame('foo', $items[0]->name);
    }

    public function testQualityDegradesTwiceAsFastWhenSellDateHasPassed(): void
    {
        //Given
        $items = [new Item('foo', 0, 5)];
        $gildedRose = new GildedRose($items);

        //When
        $gildedRose->updateQuality();

        //Then
        $this->assertSame(3, $items[0]->quality);
    }

    public function testQualityOfItemIsNeverNegative(): void
    {
        //Given
        $items = [new Item('foo', 0, 0)];
        $gildedRose = new GildedRose($items);

        //When
        $gildedRose->updateQuality();

        //Then
        $this->assertSame(0, $items[0]->quality);
    }

    /**
     * @dataProvider itemsThatIncreasesQualityWhenGetsOlder
     */
    public function testItemIncreasesQualityWhenGetsOlder(
        string $name,
        int $sellIn,
        int $resultSellIn,
        int $quality,
        int $resultQuality
    ): void {
        //Given
        $items = [new Item($name, $sellIn, $quality)];
        $gildedRose = new GildedRose($items);

        //When
        $gildedRose->updateQuality();

        //Then
        $this->assertSame($resultQuality, $items[0]->quality);
        $this->assertSame($resultSellIn, $items[0]->sell_in);
    }

    public function itemsThatIncreasesQualityWhenGetsOlder(): array
    {
        return [
            [
                'name' => 'Aged Brie',
                'sell_in' => 10,
                'result_sell_in' => 9,
                'quality' => 5,
                'result_quality' => 6
            ],
            'Quality increases by 2 when there are 10 days or less' => [
                'name' => 'Backstage passes to a TAFKAL80ETC concert',
                'sell_in' => 10,
                'result_sell_in' => 9,
                'quality' => 5,
                'result_quality' => 7
            ],
            'Quality increases by 3 when there are 5 days or less' => [
                'name' => 'Backstage passes to a TAFKAL80ETC concert',
                'sell_in' => 5,
                'result_sell_in' => 4,
                'quality' => 5,
                'result_quality' => 8
            ],
            'Quality drops to 0 after the concert' => [
                'name' => 'Backstage passes to a TAFKAL80ETC concert',
                'sell_in' => 0,
                'result_sell_in' => -1,
                'quality' => 5,
                'result_quality' => 0
            ],
        ];
    }

    public function testQualityOfItemIsNeverAboveFifty(): void
    {
        //Given
        $items = [new Item('Aged Brie', 5, 50)];
        $gildedRose = new GildedRose($items);

        //When
        $gildedRose->updateQuality();

        //Then
        $this->assertSame(50, $items[0]->quality);
    }

    public function testItemSulfurasNeverChangeItsProperties(): void
    {
        //Given
        $items = [new Item('Sulfuras, Hand of Ragnaros', 5, 5)];
        $gildedRose = new GildedRose($items);

        //When
        $gildedRose->updateQuality();

        //Then
        $this->assertSame(5, $items[0]->quality);
        $this->assertSame(5, $items[0]->sell_in);
    }
}
