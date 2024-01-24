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
        $this->item->sellIn -= 1;

        if ($this->item->quality < 50) {
            $this->item->quality += 1;

            if ($this->item->sellIn <= 10 && $this->item->quality < 50) {
                $this->item->quality += 1;
            }

            if ($this->item->sellIn <= 5 && $this->item->quality < 50) {
                $this->item->quality += 1;
            }
        }

        if ($this->item->sellIn < 0) {
            $this->item->quality = 0;
        }
    }
}
