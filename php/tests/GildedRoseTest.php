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
        $items = [
            new Item('+5 Dexterity Vest', 10, 20),
            new Item('Aged Brie', 2, 0),
            new Item('Sulfuras, Hand of Ragnaros', 0, 80),
            new Item('Conjured Mana Cake', 1, 6),
        ];

        $gildedRose = new GildedRose($items);

        $gildedRose->updateQuality();

        $this->assertSame(9, $items[0]->sell_in);
        $this->assertSame(19, $items[0]->quality);

        $this->assertSame(1, $items[1]->sell_in);
        $this->assertSame(1, $items[1]->quality);

        $this->assertSame(0, $items[2]->sell_in);
        $this->assertSame(80, $items[2]->quality);

        $this->assertSame(0, $items[3]->sell_in);
        $this->assertSame(4, $items[3]->quality);
    }
}
