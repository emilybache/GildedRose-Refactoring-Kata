<?php

declare(strict_types=1);

namespace Tests;

use GildedRose\GildedRose;
use GildedRose\Item;
use PHPUnit\Framework\TestCase;

class GildedRoseTest extends TestCase
{
    //-----------------
    // normal item
    //-----------------
    public function testUpdatesNormalItemsBeforeSellDate(): void
    {
        // arrange
        $items = [new Item('normal', 5, 10)];
        $app = new GildedRose($items);

        // act
        $app->updateQuality();

        // assert
        $this->assertSame(4, $items[0]->sellIn);
        $this->assertSame(9, $items[0]->quality);
    }

    public function testUpdatesNormalItemsOnSellDate(): void
    {
        // arrange
        $items = [new Item('normal', 0, 10)];
        $app = new GildedRose($items);

        // act
        $app->updateQuality();

        // assert
        $this->assertSame(-1, $items[0]->sellIn);
        $this->assertSame(8, $items[0]->quality);
    }

    public function testUpdatesNormalItemsAfterSellDate(): void
    {
        // arrange
        $items = [new Item('normal', -5, 10)];
        $app = new GildedRose($items);

        // act
        $app->updateQuality();

        // assert
        $this->assertSame(-6, $items[0]->sellIn);
        $this->assertSame(8, $items[0]->quality);
    }

    public function testUpdatesNormalItemsWithAQualityOf0(): void
    {
        // arrange
        $items = [new Item('normal', 5, 0)];
        $app = new GildedRose($items);

        // act
        $app->updateQuality();

        // assert
        $this->assertSame(4, $items[0]->sellIn);
        $this->assertSame(0, $items[0]->quality);
    }

    //-----------------
    // Brie item
    //-----------------
    public function testUpdatesBrieItemsBeforeSellDate(): void
    {
        // arrange
        $items = [new Item('Aged Brie', 5, 10)];
        $app = new GildedRose($items);

        // act
        $app->updateQuality();

        // assert
        $this->assertSame(4, $items[0]->sellIn);
        $this->assertSame(11, $items[0]->quality);
    }

    public function testUpdatesBrieItemsBeforeSellDateWithMaximumQuality(): void
    {
        // arrange
        $items = [new Item('Aged Brie', 5, 50)];
        $app = new GildedRose($items);

        // act
        $app->updateQuality();

        // assert
        $this->assertSame(4, $items[0]->sellIn);
        $this->assertSame(50, $items[0]->quality);
    }

    public function testUpdatesBrieItemsOnSellDate(): void
    {
        // arrange
        $items = [new Item('Aged Brie', 0, 10)];
        $app = new GildedRose($items);

        // act
        $app->updateQuality();

        // assert
        $this->assertSame(-1, $items[0]->sellIn);
        $this->assertSame(12, $items[0]->quality);
    }

    public function testUpdatesBrieItemsOnSellDateNearMaximumQuality(): void
    {
        // arrange
        $items = [new Item('Aged Brie', 0, 49)];
        $app = new GildedRose($items);

        // act
        $app->updateQuality();

        // assert
        $this->assertSame(-1, $items[0]->sellIn);
        $this->assertSame(50, $items[0]->quality);
    }

    public function testUpdatesBrieItemsOnSellDateWithMaximumQuality(): void
    {
        // arrange
        $items = [new Item('Aged Brie', 0, 50)];
        $app = new GildedRose($items);

        // act
        $app->updateQuality();

        // assert
        $this->assertSame(-1, $items[0]->sellIn);
        $this->assertSame(50, $items[0]->quality);
    }

    public function testUpdatesBrieItemsAfterSellDate(): void
    {
        // arrange
        $items = [new Item('Aged Brie', -10, 10)];
        $app = new GildedRose($items);

        // act
        $app->updateQuality();

        // assert
        $this->assertSame(-11, $items[0]->sellIn);
        $this->assertSame(12, $items[0]->quality);
    }

    public function testUpdatesBrieItemsAfterSellDateWithMaximumQuality(): void
    {
        // arrange
        $items = [new Item('Aged Brie', -10, 50)];
        $app = new GildedRose($items);

        // act
        $app->updateQuality();

        // assert
        $this->assertSame(-11, $items[0]->sellIn);
        $this->assertSame(50, $items[0]->quality);
    }

    //-----------------
    // Sulfuras item
    //-----------------
    public function testUpdatesSulfurasItemsBeforeSellDate(): void
    {
        // arrange
        $items = [new Item('Sulfuras, Hand of Ragnaros', 5, 10)];
        $app = new GildedRose($items);

        // act
        $app->updateQuality();

        // assert
        $this->assertSame(5, $items[0]->sellIn);
        $this->assertSame(10, $items[0]->quality);
    }

    public function testUpdatesSulfurasItemsOnSellDate(): void
    {
        // arrange
        $items = [new Item('Sulfuras, Hand of Ragnaros', 0, 10)];
        $app = new GildedRose($items);

        // act
        $app->updateQuality();

        // assert
        $this->assertSame(0, $items[0]->sellIn);
        $this->assertSame(10, $items[0]->quality);
    }

    public function testUpdatesSulfurasItemsAfterSellDate(): void
    {
        // arrange
        $items = [new Item('Sulfuras, Hand of Ragnaros', -1, 10)];
        $app = new GildedRose($items);

        // act
        $app->updateQuality();

        // assert
        $this->assertSame(-1, $items[0]->sellIn);
        $this->assertSame(10, $items[0]->quality);
    }

    //-----------------
    // Backstage Pass
    //-----------------
    public function testUpdatesBackstagePassItemsLongBeforeSellDate(): void
    {
        // arrange
        $items = [new Item('Backstage passes to a TAFKAL80ETC concert', 11, 10)];
        $app = new GildedRose($items);

        // act
        $app->updateQuality();

        // assert
        $this->assertSame(10, $items[0]->sellIn);
        $this->assertSame(11, $items[0]->quality);
    }

    public function testUpdatesBackstagePassItemsCloseToBeforeSellDate(): void
    {
        // arrange
        $items = [new Item('Backstage passes to a TAFKAL80ETC concert', 10, 10)];
        $app = new GildedRose($items);

        // act
        $app->updateQuality();

        // assert
        $this->assertSame(9, $items[0]->sellIn);
        $this->assertSame(12, $items[0]->quality);
    }

    public function testUpdatesBackstagePassItemsCloseToSellDateAtMaximumQuality(): void
    {
        // arrange
        $items = [new Item('Backstage passes to a TAFKAL80ETC concert', 10, 50)];
        $app = new GildedRose($items);

        // act
        $app->updateQuality();

        // assert
        $this->assertSame(9, $items[0]->sellIn);
        $this->assertSame(50, $items[0]->quality);
    }

    public function testUpdatesBackstagePassItemsVeryCloseToSellDate(): void
    {
        // arrange
        $items = [new Item('Backstage passes to a TAFKAL80ETC concert', 5, 10)];
        $app = new GildedRose($items);

        // act
        $app->updateQuality();

        // assert
        $this->assertSame(4, $items[0]->sellIn);
        $this->assertSame(13, $items[0]->quality);
    }

    public function testUpdatesBackstagePassItemsVeryCloseToSellDateAtMaxiumQuality(): void
    {
        // arrange
        $items = [new Item('Backstage passes to a TAFKAL80ETC concert', 5, 50)];
        $app = new GildedRose($items);

        // act
        $app->updateQuality();

        // assert
        $this->assertSame(4, $items[0]->sellIn);
        $this->assertSame(50, $items[0]->quality);
    }

    public function testUpdatesBackstagePassItemsWithOneDayLeftToSellDateAtMaxiumQuality(): void
    {
        // arrange
        $items = [new Item('Backstage passes to a TAFKAL80ETC concert', 1, 50)];
        $app = new GildedRose($items);

        // act
        $app->updateQuality();

        // assert
        $this->assertSame(0, $items[0]->sellIn);
        $this->assertSame(50, $items[0]->quality);
    }

    public function testUpdatesBackstagePassItemsOnSellDate(): void
    {
        // arrange
        $items = [new Item('Backstage passes to a TAFKAL80ETC concert', 0, 10)];
        $app = new GildedRose($items);

        // act
        $app->updateQuality();

        // assert
        $this->assertSame(-1, $items[0]->sellIn);
        $this->assertSame(0, $items[0]->quality);
    }

    public function testUpdatesBackstagePassItemsAfterSellDate(): void
    {
        // arrange
        $items = [new Item('Backstage passes to a TAFKAL80ETC concert', -1, 10)];
        $app = new GildedRose($items);

        // act
        $app->updateQuality();

        // assert
        $this->assertSame(-2, $items[0]->sellIn);
        $this->assertSame(0, $items[0]->quality);
    }
}
