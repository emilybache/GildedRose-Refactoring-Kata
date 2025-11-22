<?php

declare(strict_types=1);

namespace Tests;

use GildedRose\GildedRose;
use GildedRose\Item;
use PHPUnit\Framework\TestCase;

class GildedRoseTest extends TestCase
{
    public function test_item_should_increase_quality_by_2(): void
    {
        $items = [new Item('item', 0,80)];
        $sut = new GildedRose($items);

        $sut->updateQuality();

        $this->assertSame(78, $items[0]->quality);
    }

    public function test_quality_never_be_negative(): void
    {
        $items = [new Item('item', 0,0)];
        $sut = new GildedRose($items);

        $sut->updateQuality();

        $this->assertSame(0, $items[0]->quality);
    }

    public function test_item_should_sellable(): void
    {
        $items = [new Item('item', 2,80)];
        $sut = new GildedRose($items);

        $sut->updateQuality();

        $this->assertSame(1, $items[0]->sellIn);
    }


    public function test_aged_brie_quality_is_increasable(): void
    {
        $items = [new Item('Aged Brie', 60,20)];
        $sut = new GildedRose($items);

        $sut->updateQuality();

        $this->assertSame(21, $items[0]->quality);
    }

    public function test_max_quality_is_50(): void
    {
        $items = [new Item('Aged Brie', 0,50)];
        $sut = new GildedRose($items);

        $sut->updateQuality();

        $this->assertSame(50, $items[0]->quality);
    }
    public function test_aged_brie_is_sellable(): void
    {
        $items = [new Item('Aged Brie', 60,20)];
        $sut = new GildedRose($items);

        $sut->updateQuality();

        $this->assertSame(59, $items[0]->sellIn);
    }



    public function test_sulfuras_never_ruined(): void
    {
        $items = [new Item('Sulfuras, Hand of Ragnaros', 0,80)];
        $sut = new GildedRose($items);

        $sut->updateQuality();

        $this->assertSame(80, $items[0]->quality);
    }

    public function test_sulfuras_is_not_sellable(): void
    {
        $items = [new Item('Sulfuras, Hand of Ragnaros', 0,80)];
        $sut = new GildedRose($items);

        $sut->updateQuality();

        $this->assertSame(0, $items[0]->sellIn);
    }



    public function test_backstage_passes_quality_increases_by_2(): void
    {
        $items = [new Item('Backstage passes to a TAFKAL80ETC concert', 10,20)];
        $sut = new GildedRose($items);

        $sut->updateQuality();

        $this->assertSame(22, $items[0]->quality);
    }

    public function test_backstage_passes_quality_increases_by_3(): void
    {
        $items = [new Item('Backstage passes to a TAFKAL80ETC concert', 5,20)];
        $sut = new GildedRose($items);

        $sut->updateQuality();

        $this->assertSame(23, $items[0]->quality);
    }

    public function test_backstage_passes_quality_dropped_to_zero(): void
    {
        $items = [new Item('Backstage passes to a TAFKAL80ETC concert', 0,20)];
        $sut = new GildedRose($items);

        $sut->updateQuality();

        $this->assertSame(0, $items[0]->quality);
    }

    public function test_backstage_passes_is_sellable(): void
    {
        $items = [new Item('Backstage passes to a TAFKAL80ETC concert', 5,20)];
        $sut = new GildedRose($items);

        $sut->updateQuality();

        $this->assertSame(4, $items[0]->sellIn);
    }
}