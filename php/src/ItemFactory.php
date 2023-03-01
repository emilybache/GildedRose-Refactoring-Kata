<?php

declare(strict_types=1);

namespace GildedRose;

use GildedRose\Items\Interface\ItemInterface;
use GildedRose\Items\AgedBrieItem;
use GildedRose\Items\BackstagePassItem;
use GildedRose\Items\ConjuredItem;
use GildedRose\Items\NormalItem;
use GildedRose\Items\SulfurasItem;
class ItemFactory
{
    public const AGED_BRIE_ITEM = 'Aged Brie';
    public const BACKSTAGE_ITEM = 'Backstage';
    public const SULFURAS_ITEM = 'Sulfuras';
    public const CONJURED_ITEM = 'Conjured';

    /**
     * @param Item $item
     *
     * @return ItemInterface
     */
    public static function createItem(Item $item): ItemInterface
    {
        return match(true) {
            strpos($item->name, self::AGED_BRIE_ITEM) !== false => new AgedBrieItem(),
            strpos($item->name, self::BACKSTAGE_ITEM) !== false => new BackstagePassItem(),
            strpos($item->name, self::SULFURAS_ITEM) !== false => new SulfurasItem(),
            strpos($item->name, self::CONJURED_ITEM) !== false => new ConjuredItem(),
            default => new NormalItem()
        };
    }

}
