<?php

declare(strict_types=1);

namespace PhpUnitTests\GildedRoseItem;

use GildedRose\GildedRoseItem\SulfurasItem;
use GildedRose\Item;
use PHPUnit\Framework\TestCase;

class SulfurasItemTest extends TestCase
{
    public function testTimelessness(): void
    {
        $item = new Item(SulfurasItem::NAME, 20, 20);
        $gildedRoseItem = new SulfurasItem($item);
        $gildedRoseItem->ageByOneDay();

        $this->assertEquals(20, $item->quality);
        $this->assertEquals(20, $item->sellIn);
    }
}
