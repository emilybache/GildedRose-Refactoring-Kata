<?php

declare(strict_types=1);

namespace GildedRose;

use GildedRose\GildedRoseItem\BackstagePassItem;
use GildedRose\GildedRoseItem\BrieItem;
use GildedRose\GildedRoseItem\ConjuredItem;
use GildedRose\GildedRoseItem\DefaultItem;
use GildedRose\GildedRoseItem\SulfurasItem;

class GildedRoseItemFactory
{
    public function createGildedRoseItem(Item $item): GildedRoseItem
    {
        switch ($item->name) {
            case BrieItem::NAME:
                return new BrieItem($item);
            case BackstagePassItem::NAME:
                return new BackstagePassItem($item);
            case SulfurasItem::NAME:
                return new SulfurasItem($item);
            case ConjuredItem::NAME:
                return new ConjuredItem($item);
            default:
                return new DefaultItem($item);
        }
    }
}
