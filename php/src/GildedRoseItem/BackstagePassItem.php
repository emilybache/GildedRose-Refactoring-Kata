<?php

declare(strict_types=1);

namespace GildedRose\GildedRoseItem;

use GildedRose\GildedRoseItem;
use GildedRose\Item;

class BackstagePassItem implements GildedRoseItem
{
    public const NAME = 'Backstage passes to a TAFKAL80ETC concert';

    public function __construct(
        private Item $item
    ) {
    }

    public function ageByOneDay(): void
    {
        --$this->item->sellIn;
        $this->item->quality = min(50, $this->getNewQuality($this->item->quality, $this->item->sellIn));
    }

    private function getNewQuality(int $previousQuality, int $sellIn): int
    {
        if ($sellIn < 0) {
            return 0;
        }

        if ($sellIn < 5) {
            return $previousQuality + 3;
        }

        if ($sellIn < 10) {
            return $previousQuality + 2;
        }

        return $previousQuality + 1;
    }
}
