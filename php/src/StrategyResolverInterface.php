<?php

namespace GildedRose;

use GildedRose\QualityStrategy\QualityStrategyInterface;
use GildedRose\SellInStrategy\SellInStrategyInterface;

interface StrategyResolverInterface
{
    public const ITEM_NAME_AGED_BRIE = 'Aged Brie';

    public const ITEM_NAME_SULFURAS = 'Sulfuras, Hand of Ragnaros';

    public const ITEM_NAME_BACKSTAGE_PASSES = 'Backstage passes to a TAFKAL80ETC concert';

    public const ITEM_NAME_CONJURED = 'Conjured Mana Cake';

    public function getSellInStrategy(Item $item): SellInStrategyInterface;

    public function getQualityStrategy(Item $item): QualityStrategyInterface;
}
