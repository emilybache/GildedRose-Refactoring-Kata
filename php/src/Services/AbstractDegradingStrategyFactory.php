<?php

namespace GildedRose\Services;

use GildedRose\Item;
use GildedRose\Services\Strategies\BaseStrategy;

interface AbstractDegradingStrategyFactory
{
    public const AGED = 'Aged Brie';

    public const SULFURAS = 'Sulfuras, Hand of Ragnaros';

    public const BACKSTAGE = 'Backstage passes to a TAFKAL80ETC concert';

    public const CONJURED = 'Conjured Mana Cake';

    public function getDegradingStrategy(Item $item): BaseStrategy;
}
