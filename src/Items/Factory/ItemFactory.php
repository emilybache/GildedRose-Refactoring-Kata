<?php

declare(strict_types=1);

namespace GildedRose\Items\Factory;

use GildedRose\Item;
use GildedRose\Items\AgedBrieItem;
use GildedRose\Items\BackstagePassItem;
use GildedRose\Items\ConjuredItem;
use GildedRose\Items\Interface\ItemInterface;
use GildedRose\Items\NormalItem;
use GildedRose\Items\SulfurasItem;

class ItemFactory
{
    public const AGED_BRIE_ITEM = 'Aged Brie';

    public const BACKSTAGE_ITEM = 'Backstage';

    public const SULFURAS_ITEM = 'Sulfuras';

    public const CONJURED_ITEM = 'Conjured';

    public static function createItem(Item $item): ItemInterface
    {
        return match (true) {
            str_contains($item->name, self::AGED_BRIE_ITEM) => new AgedBrieItem(),
            str_contains($item->name, self::BACKSTAGE_ITEM) => new BackstagePassItem(),
            str_contains($item->name, self::SULFURAS_ITEM) => new SulfurasItem(),
            str_contains($item->name, self::CONJURED_ITEM) => new ConjuredItem(),
            default => new NormalItem()
        };
    }
}
