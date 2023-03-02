<?php

declare(strict_types=1);

namespace Tests;

use GildedRose\GildedRose;
use GildedRose\Item;
use PHPUnit\Framework\TestCase;

class GildedRoseTest extends TestCase
{
    public function testQualityDegradesTwiceWhenSellInNegative(): void
    {
        $items = [
            new Item('Normal Item', 0, 12),
        ];
        $GildedRose = new GildedRose($items);
        $GildedRose->updateQuality();
        $this->assertEquals(-1, $items[0]->sellIn);
        $this->assertEquals(10, $items[0]->quality);
    }

    public function testWithQualityNeverNagative(): void
    {
        $items = [
            new Item('Normal Item', 9, 0),
        ];
        $GildedRose = new GildedRose($items);
        $GildedRose->updateQuality();
        $this->assertEquals(8, $items[0]->sellIn);
        $this->assertEquals(0, $items[0]->quality);
    }

    public function testWithQualityNeverGreaterThan50(): void
    {
        $items = [
            new Item('Aged Brie', 9, 50),
        ];
        $GildedRose = new GildedRose($items);
        $GildedRose->updateQuality();
        $this->assertEquals(8, $items[0]->sellIn);
        $this->assertEquals(50, $items[0]->quality);
    }

    public function testAgedBrieIncreaseQualityWhenOlder(): void
    {
        $items = [
            new Item('Aged Brie', 10, 10),
        ];
        $GildedRose = new GildedRose($items);
        $GildedRose->updateQuality();
        $this->assertEquals(9, $items[0]->sellIn);
        $this->assertEquals(11, $items[0]->quality);
    }

    public function testAgedBrieIncraseQualityTwiceWhenSellInNagative(): void
    {
        $items = [
            new Item('Aged Brie', -1, 10),
        ];
        $GildedRose = new GildedRose($items);
        $GildedRose->updateQuality();
        $this->assertEquals(-2, $items[0]->sellIn);
        $this->assertEquals(12, $items[0]->quality);
    }

    public function testSulfurasNeverChangeQuality(): void
    {
        $items = [
            new Item('Sulfuras', -1, 10),
        ];
        $GildedRose = new GildedRose($items);
        $GildedRose->updateQuality();
        $this->assertEquals(-2, $items[0]->sellIn);
        $this->assertEquals(10, $items[0]->quality);
    }

    public function testBackstageIncreaseQualityBy2WhenSellInLessThanOrEqual10(): void
    {
        $items = [
            new Item('Backstage passes', 9, 10),
        ];
        $GildedRose = new GildedRose($items);
        $GildedRose->updateQuality();
        $this->assertEquals(8, $items[0]->sellIn);
        $this->assertEquals(12, $items[0]->quality);
    }

    public function testBackstageIncreaseQualityBy3WhenSellInLessThanOrEqual5(): void
    {
        $items = [
            new Item('Backstage passes', 4, 10),
        ];
        $GildedRose = new GildedRose($items);
        $GildedRose->updateQuality();
        $this->assertEquals(3, $items[0]->sellIn);
        $this->assertEquals(13, $items[0]->quality);
    }

    public function testBackstageSetQualityZeroWhenSellInLessThanOrEqualZero(): void
    {
        $items = [
            new Item('Backstage passes', 0, 10),
        ];
        $GildedRose = new GildedRose($items);
        $GildedRose->updateQuality();
        $this->assertEquals(-1, $items[0]->sellIn);
        $this->assertEquals(0, $items[0]->quality);
    }

    public function testConjuredDegradesQualityTwiceFastAsNormalItem(): void
    {
        $items = [
            new Item('Conjured', 10, 10),
        ];
        $GildedRose = new GildedRose($items);
        $GildedRose->updateQuality();
        $this->assertEquals(9, $items[0]->sellIn);
        $this->assertEquals(8, $items[0]->quality);
    }

    public function testUpdateSellInAndQualityWith10Days(): void
    {
        $dayTest = 10;
        $items = $this->getPayloadItems();
        $GildedRose = new GildedRose($items);

        for ($i = 1; $i < $dayTest; $i++) {
            $GildedRose->updateQuality();
        }

        $this->assertEquals(1, $items[0]->sellIn);
        $this->assertEquals(-7, $items[1]->sellIn);
        $this->assertEquals(-4, $items[2]->sellIn);
        $this->assertEquals(-9, $items[3]->sellIn);
        $this->assertEquals(-10, $items[4]->sellIn);
        $this->assertEquals(6, $items[5]->sellIn);
        $this->assertEquals(1, $items[6]->sellIn);
        $this->assertEquals(-4, $items[7]->sellIn);
        $this->assertEquals(-6, $items[8]->sellIn);

        $this->assertEquals(11, $items[0]->quality);
        $this->assertEquals(16, $items[1]->quality);
        $this->assertEquals(0, $items[2]->quality);
        $this->assertEquals(80, $items[3]->quality);
        $this->assertEquals(80, $items[4]->quality);
        $this->assertEquals(34, $items[5]->quality);
        $this->assertEquals(50, $items[6]->quality);
        $this->assertEquals(0, $items[7]->quality);
        $this->assertEquals(0, $items[8]->quality);
    }

    public function testUpdateSellInAndQualityWith31Days(): void
    {
        $dayTest = 31;
        $items = $this->getPayloadItems();
        $GildedRose = new GildedRose($items);

        for ($i = 1; $i < $dayTest; $i++) {
            $GildedRose->updateQuality();
        }

        $this->assertEquals(-20, $items[0]->sellIn);
        $this->assertEquals(-28, $items[1]->sellIn);
        $this->assertEquals(-25, $items[2]->sellIn);
        $this->assertEquals(-30, $items[3]->sellIn);
        $this->assertEquals(-31, $items[4]->sellIn);
        $this->assertEquals(-15, $items[5]->sellIn);
        $this->assertEquals(-20, $items[6]->sellIn);
        $this->assertEquals(-25, $items[7]->sellIn);
        $this->assertEquals(-27, $items[8]->sellIn);

        $this->assertEquals(0, $items[0]->quality);
        $this->assertEquals(50, $items[1]->quality);
        $this->assertEquals(0, $items[2]->quality);
        $this->assertEquals(80, $items[3]->quality);
        $this->assertEquals(80, $items[4]->quality);
        $this->assertEquals(0, $items[5]->quality);
        $this->assertEquals(0, $items[6]->quality);
        $this->assertEquals(0, $items[7]->quality);
        $this->assertEquals(0, $items[8]->quality);
    }

    private function getPayloadItems(): array
    {
        return [
            new Item('+5 Dexterity Vest', 10, 20),
            new Item('Aged Brie', 2, 0),
            new Item('Elixir of the Mongoose', 5, 7),
            new Item('Sulfuras, Hand of Ragnaros', 0, 80),
            new Item('Sulfuras, Hand of Ragnaros', -1, 80),
            new Item('Backstage passes to a TAFKAL80ETC concert', 15, 20),
            new Item('Backstage passes to a TAFKAL80ETC concert', 10, 49),
            new Item('Backstage passes to a TAFKAL80ETC concert', 5, 49),
            new Item('Conjured Mana Cake', 3, 6),
        ];
    }
}
