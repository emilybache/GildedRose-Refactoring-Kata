<?php

declare(strict_types=1);

namespace GildedRose;

final class GildedRose
{
    /**
     * @param Item[] $items
     */
    public function __construct(
        private array $items
    )
    {
    }

    public function updateQuality(): void
    {
        foreach ($this->items as $item) {
            if ($item->name === 'Sulfuras, Hand of Ragnaros')
                continue;

            $item->sellIn--;

            if ($item->name === 'Aged Brie') {
                $item->quality = min(50, ++$item->quality);
            } else if ($item->name === 'Backstage passes to a TAFKAL80ETC concert') {
                if ($item->sellIn < 0) {
                    $item->quality = 0;
                } else if ($item->sellIn <= 5) {
                    $item->quality = min(50, $item->quality + 3);
                } else if ($item->sellIn <= 10) {
                    $item->quality = min(50, $item->quality + 2);
                }
            } else {
                $item->quality = max(0, $item->quality - 2);
            }
        }
    }

}
