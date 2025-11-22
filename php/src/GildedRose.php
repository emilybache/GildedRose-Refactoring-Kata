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

            if ($item->name === 'Sulfuras, Hand of Ragnaros') {
                continue;
            }


            if ($item->name != 'Aged Brie' and $item->name != 'Backstage passes to a TAFKAL80ETC concert') {
                $item->quality = max(0, --$item->quality);
            } else {
                if ($item->quality < 50) {
                    $item->quality = $item->quality + 1;
                    if ($item->name == 'Backstage passes to a TAFKAL80ETC concert') {
                        if ($item->sellIn < 11) {
                            if ($item->quality < 50) {
                                $item->quality = $item->quality + 1;
                            }
                        }
                        if ($item->sellIn < 6) {
                            if ($item->quality < 50) {
                                $item->quality = $item->quality + 1;
                            }
                        }
                    }
                }
            }


            $item->sellIn--;

            if ($item->sellIn < 0) {
                if ($item->name === 'Aged Brie') {
                    $item->quality = min(50, ++$item->quality);
                } else if ($item->name === 'Backstage passes to a TAFKAL80ETC concert')
                    $item->quality = 0;
                else
                    $item->quality = max(0, --$item->quality);
            }
        }
    }
}
