<?php

declare(strict_types=1);

namespace GildedRose\GildedRoseItem;

use GildedRose\GildedRoseItem;
use GildedRose\Item;

class BackstagePassItem implements GildedRoseItem
{
    public const NAME = 'Backstage passes to a TAFKAL80ETC concert';

    public function __construct(private Item $item) {}

    public function ageByOneDay(): void
    {
    }
}
