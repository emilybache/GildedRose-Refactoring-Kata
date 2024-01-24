<?php

declare(strict_types=1);

namespace GildedRose\GildedRoseItem;

use GildedRose\GildedRoseItem;
use GildedRose\Item;

class ConjuredItem implements GildedRoseItem
{
    public const NAME = 'Conjured Mana Cake';

    public function __construct(
        private Item $item
    ) {
    }

    public function ageByOneDay(): void
    {
        --$this->item->sellIn;
        $this->item->quality = max(0, $this->getNewQuality($this->item->quality, $this->item->sellIn));
    }

    private function getNewQuality(int $previousQuality, int $sellIn): int
    {
        if ($sellIn < 0) {
            return $previousQuality - 4;
        }

        return $previousQuality - 2;
    }
}
