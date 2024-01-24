<?php

declare(strict_types=1);

namespace PhpUnitTests\GildedRoseItem;

use GildedRose\GildedRoseItem\ConjuredItem;
use GildedRose\Item;
use PHPUnit\Framework\TestCase;

class ConjuredItemTest extends TestCase
{
    public function testNormalQualityReduction(): void
    {
        $item = new Item(ConjuredItem::NAME, 20, 20);
        $gildedRoseItem = new ConjuredItem($item);
        $gildedRoseItem->ageByOneDay();

        $this->assertEquals(18, $item->quality);
        $this->assertEquals(19, $item->sellIn);
    }

    public function testQualityCantBeNegative(): void
    {
        $item = new Item(ConjuredItem::NAME, 20, 0);
        $gildedRoseItem = new ConjuredItem($item);
        $gildedRoseItem->ageByOneDay();

        $this->assertEquals(0, $item->quality);
    }

    public function testQualityDegradesTwiceAsFastAsDefault(): void
    {
        $item = new Item(ConjuredItem::NAME, 20, 20);
        $gildedRoseItem = new ConjuredItem($item);
        $gildedRoseItem->ageByOneDay();

        $this->assertEquals(18, $item->quality);
    }

    public function testQualityDegradesTwiceAsFastAfterSellBy(): void
    {
        $item = new Item(ConjuredItem::NAME, 0, 20);
        $gildedRoseItem = new ConjuredItem($item);
        $gildedRoseItem->ageByOneDay();

        $this->assertEquals(16, $item->quality);
    }
}
