<?php

declare(strict_types=1);

namespace GildedRose\GildedRoseItem;

use GildedRose\GildedRoseItem;
use GildedRose\Item;

class BrieItem implements GildedRoseItem
{
    public const NAME = 'Aged Brie';

    public function __construct(private Item $item) {}

    public function ageByOneDay(): void
    {
    }
}
