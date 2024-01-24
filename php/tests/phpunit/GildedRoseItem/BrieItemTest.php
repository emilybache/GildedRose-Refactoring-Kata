<?php

declare(strict_types=1);

namespace PhpUnitTests\GildedRoseItem;

use GildedRose\GildedRoseItem\BrieItem;
use GildedRose\Item;
use PHPUnit\Framework\TestCase;

class BrieItemTest extends TestCase
{
    public function testQualityIncreases(): void
    {
        $item = new Item(BrieItem::NAME, 20, 20);
        $gildedRoseItem = new BrieItem($item);
        $gildedRoseItem->ageByOneDay();

        $this->assertEquals(21, $item->quality);
    }

    public function testQualityCantGoOver50(): void
    {
        $item = new Item(BrieItem::NAME, 20, 50);
        $gildedRoseItem = new BrieItem($item);
        $gildedRoseItem->ageByOneDay();

        $this->assertEquals(50, $item->quality);
    }
}
