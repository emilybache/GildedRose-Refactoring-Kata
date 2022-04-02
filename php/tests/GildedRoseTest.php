<?php

declare(strict_types=1);

namespace Tests;

use GildedRose\GildedRose;
use GildedRose\Item;
use PHPUnit\Framework\TestCase;

class GildedRoseTest extends TestCase
{
    public function testQualityNeverIsNegative(): void
    {
        $items = [new Item("foo", 0, 0)];
        $app = new GildedRose($items);

        $app->updateQuality();

        $this->assertSame(0, $app->getItems()[0]->quality);
    }

    public function testSulfurasCouldNotBeSold(): void
    {
        $items = [new Item("Sulfuras, Hand of Ragnaros", 10, 0)];
        $app = new GildedRose($items);

        $app->updateQuality();

        $this->assertSame(10, $app->getItems()[0]->sellIn);
    }

    public function testSulfurasCouldNotDecreaseQuality(): void
    {
        $items = [new Item("Sulfuras, Hand of Ragnaros", 10, 10)];
        $app = new GildedRose($items);

        $app->updateQuality();

        $this->assertSame(10, $app->getItems()[0]->quality);
    }

    public function testQualityCouldNotBeMoreThanFifty(): void
    {
        $items = [new Item("Aged Brie", 10, 50)];
        $app = new GildedRose($items);

        $app->updateQuality();

        $this->assertSame(50, $app->getItems()[0]->quality);
    }

    public function testItemWithDatePassedQualityDecreaseByTwice(): void
    {
        $items = [new Item("foo", -1, 40)];
        $app = new GildedRose($items);

        $app->updateQuality();

        $this->assertSame(38, $app->getItems()[0]->quality);
    }

    public function testAgedBrieIncreaseQualityWhenItGetsOlder(): void
    {
        $items = [new Item("Aged Brie", 1, 40)];
        $app = new GildedRose($items);

        $app->updateQuality();

        $this->assertSame(41, $app->getItems()[0]->quality);
    }

    public function testAgedBrieIncreaseByTwoQualityWhenDatePassed(): void
    {
        $items = [new Item("Aged Brie", -1, 40)];
        $app = new GildedRose($items);

        $app->updateQuality();

        $this->assertSame(42, $app->getItems()[0]->quality);
    }

    public function testAgedBrieIncreaseByTwoQualityWhenDatePassedAndNotMoreThanFifty() {
        $items = [new Item("Aged Brie", -1, 50)];
        $app = new GildedRose($items);

        $app->updateQuality();

        $this->assertSame(50, $app->getItems()[0]->quality);
    }

    public function testBackstagePassesIncreaseQualityByTwoWhenSelinLessThanTen() {
        $items = [new Item("Backstage passes to a TAFKAL80ETC concert", 10, 40)];
        $app = new GildedRose($items);

        $app->updateQuality();

        $this->assertSame(42, $app->getItems()[0]->quality);
    }

    public function testBackstagePassesIncreaseQualityByTwoWhenSellinLessThanSix() {
        $items = [new Item("Backstage passes to a TAFKAL80ETC concert", 6, 40)];
        $app = new GildedRose($items);

        $app->updateQuality();

        $this->assertSame(42, $app->getItems()[0]->quality);
    }

    public function testBackstagePassesIncreaseQualityByThreeWhenSellinLessThanFive() {
        $items = [new Item("Backstage passes to a TAFKAL80ETC concert", 5, 40)];
        $app = new GildedRose($items);

        $app->updateQuality();

        $this->assertSame(43, $app->getItems()[0]->quality);
    }

    public function testBackstagePassesIncreaseQualityByTwoWhenSellinLessThanSixAndNotMoreThanFifty() {
        $items = [new Item("Backstage passes to a TAFKAL80ETC concert", 6, 49)];
        $app = new GildedRose($items);

        $app->updateQuality();

        $this->assertSame(50, $app->getItems()[0]->quality);
    }

    public function testBackstagePassesIncreaseQualityByThreeWhenSellinLessThanFiveAndNotMoreThanFifty() {
        $items = [new Item("Backstage passes to a TAFKAL80ETC concert", 5, 48)];
        $app = new GildedRose($items);

        $app->updateQuality();

        $this->assertSame(50, $app->getItems()[0]->quality);
    }

    public function testBackstagePassesQualityDropsToZeroAfterConcert() {
        $items = [new Item("Backstage passes to a TAFKAL80ETC concert", 0, 40)];
        $app = new GildedRose($items);

        $app->updateQuality();

        $this->assertSame(0, $app->getItems()[0]->quality);
    }

    public function testBackstagePassesQualityIncreaseQualityByOneWhenDateIsMoreThanTen() {
        $items = [new Item("Backstage passes to a TAFKAL80ETC concert", 11, 40)];
        $app = new GildedRose($items);

        $app->updateQuality();

        $this->assertSame(41, $app->getItems()[0]->quality);
    }
}
