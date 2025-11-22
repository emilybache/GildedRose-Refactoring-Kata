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
            

            if ($item->name === 'Aged Brie') {
                $item->quality = min(50, ++$item->quality);
            } else if ($item->name === 'Backstage passes to a TAFKAL80ETC concert') {
                if ($item->sellIn <= 5) {
                    $updatedQuality = $item->quality + 3;
                    $item->quality = min(50, $updatedQuality);
                } else if ($item->sellIn <= 10) {
                    $updatedQuality = $item->quality + 2;
                    $item->quality = min(50, $updatedQuality);
                }
            } else {
                $updatedQuality = $item->quality - 2;
                $item->quality = max(0, $updatedQuality);
            }

            $item->sellIn--;

            if ($item->sellIn < 0 && $item->name === 'Backstage passes to a TAFKAL80ETC concert')
                $item->quality = 0;

        }
    }
}
