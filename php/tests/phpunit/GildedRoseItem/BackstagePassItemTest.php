<?php

declare(strict_types=1);

namespace PhpUnitTests\GildedRoseItem;

use GildedRose\GildedRoseItem\BackstagePassItem;
use GildedRose\Item;
use PHPUnit\Framework\TestCase;

class BackstagePassItemTest extends TestCase
{
    public function testQualityIncreases(): void
    {
        $item = new Item(BackstagePassItem::NAME, 20, 20);
        $gildedRoseItem = new BackstagePassItem($item);
        $gildedRoseItem->ageByOneDay();

        $this->assertEquals(21, $item->quality);
    }

    public function testQualityIncreasesFasterUnder10Days(): void
    {
        $item = new Item(BackstagePassItem::NAME, 10, 20);
        $gildedRoseItem = new BackstagePassItem($item);
        $gildedRoseItem->ageByOneDay();

        $this->assertEquals(22, $item->quality);
    }

    public function testQualityIncreasesEvenFasterUnder5Days(): void
    {
        $item = new Item(BackstagePassItem::NAME, 5, 20);
        $gildedRoseItem = new BackstagePassItem($item);
        $gildedRoseItem->ageByOneDay();

        $this->assertEquals(23, $item->quality);
    }

    public function testQualityIsZeroAfterConcert(): void
    {
        $item = new Item(BackstagePassItem::NAME, 0, 20);
        $gildedRoseItem = new BackstagePassItem($item);
        $gildedRoseItem->ageByOneDay();

        $this->assertEquals(0, $item->quality);
    }
}
