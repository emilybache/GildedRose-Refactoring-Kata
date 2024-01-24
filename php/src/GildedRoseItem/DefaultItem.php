<?php

declare(strict_types=1);

namespace GildedRose\GildedRoseItem;

use GildedRose\GildedRoseItem;
use GildedRose\Item;

class DefaultItem implements GildedRoseItem
{
    public function __construct(private Item $item) {}

    public function ageByOneDay(): void
    {
        $this->item->sellIn -= 1;

        if ($this->item->quality >= 1) {
            $this->item->quality -= 1;
        }
    }
}
