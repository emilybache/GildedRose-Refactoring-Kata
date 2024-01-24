<?php

declare(strict_types=1);

namespace PhpUnitTests\GildedRoseItem;

use GildedRose\GildedRoseItem\DefaultItem;
use GildedRose\Item;
use PHPUnit\Framework\TestCase;

class DefaultItemTest extends TestCase
{
    public function testNormalQualityReduction(): void
    {
        $item = new Item('foo', 20, 20);
        $gildedRoseItem = new DefaultItem($item);
        $gildedRoseItem->ageByOneDay();

        $this->assertEquals(19, $item->quality);
        $this->assertEquals(19, $item->sellIn);
    }

    public function testQualityCantBeNegative(): void
    {
        $item = new Item('foo', 20, 0);
        $gildedRoseItem = new DefaultItem($item);
        $gildedRoseItem->ageByOneDay();

        $this->assertEquals(0, $item->quality);
    }

    public function testQualityDegradesTwiceAsFastAfterSellBy(): void
    {
        $item = new Item('foo', 0, 20);
        $gildedRoseItem = new DefaultItem($item);
        $gildedRoseItem->ageByOneDay();

        $this->assertEquals(18, $item->quality);
    }
}
