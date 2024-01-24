<?php

declare(strict_types=1);

namespace PhpUnitTests;

use GildedRose\GildedRoseItem;
use GildedRose\GildedRoseItem\BackstagePassItem;
use GildedRose\GildedRoseItem\BrieItem;
use GildedRose\GildedRoseItem\ConjuredItem;
use GildedRose\GildedRoseItem\DefaultItem;
use GildedRose\GildedRoseItem\SulfurasItem;
use GildedRose\GildedRoseItemFactory;
use GildedRose\Item;
use PHPUnit\Framework\TestCase;

class GildedRoseItemFactoryTest extends TestCase
{
    private GildedRoseItemFactory $factory;

    protected function setUp(): void
    {
        $this->factory = new GildedRoseItemFactory();
    }

    public function testCreatingDefaultItem(): void
    {
        $item = new Item('foo', 0, 0);
        $gildedRoseItem = $this->factory->createGildedRoseItem($item);

        $this->assertInstanceOf(GildedRoseItem::class, $gildedRoseItem);
        $this->assertInstanceOf(DefaultItem::class, $gildedRoseItem);
    }

    public function testCreatingBrieItem(): void
    {
        $item = new Item('Aged Brie', 0, 0);
        $gildedRoseItem = $this->factory->createGildedRoseItem($item);

        $this->assertInstanceOf(GildedRoseItem::class, $gildedRoseItem);
        $this->assertInstanceOf(BrieItem::class, $gildedRoseItem);
    }

    public function testCreatingBackstagePassItem(): void
    {
        $item = new Item('Backstage passes to a TAFKAL80ETC concert', 0, 0);
        $gildedRoseItem = $this->factory->createGildedRoseItem($item);

        $this->assertInstanceOf(GildedRoseItem::class, $gildedRoseItem);
        $this->assertInstanceOf(BackstagePassItem::class, $gildedRoseItem);
    }

    public function testCreatingSulfuraItem(): void
    {
        $item = new Item('Sulfuras, Hand of Ragnaros', 0, 0);
        $gildedRoseItem = $this->factory->createGildedRoseItem($item);

        $this->assertInstanceOf(GildedRoseItem::class, $gildedRoseItem);
        $this->assertInstanceOf(SulfurasItem::class, $gildedRoseItem);
    }

    public function testCreatingConjuredItem(): void
    {
        $item = new Item('Conjured Mana Cake', 0, 0);
        $gildedRoseItem = $this->factory->createGildedRoseItem($item);

        $this->assertInstanceOf(GildedRoseItem::class, $gildedRoseItem);
        $this->assertInstanceOf(ConjuredItem::class, $gildedRoseItem);
    }
}
