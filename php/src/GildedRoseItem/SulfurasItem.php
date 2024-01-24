<?php

declare(strict_types=1);

namespace GildedRose\GildedRoseItem;

use GildedRose\GildedRoseItem;
use GildedRose\Item;

class SulfurasItem implements GildedRoseItem
{
    public const NAME = 'Sulfuras, Hand of Ragnaros';

    public function __construct(
        private Item $item
    ) {
    }

    public function ageByOneDay(): void
    {
    }
}
