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
    ) {
    }

    public function getItems(): array
    {
        return $this->items;
    }

    public function updateQuality(): void
    {
        foreach ($this->items as $item) {

            $name = $item->name;
            if(str_starts_with(strtolower($name), 'conjured')) {
                $name = 'conjured';
            }

            switch ($name) {
                case 'Aged Brie':
                    $item->sellIn--;
                    if ($item->quality < 50) {
                        $item->quality++;
                    }
                    break;
                case 'Backstage passes to a TAFKAL80ETC concert':

                    if (($item->quality < 50) && ($item->sellIn >= 0)) {
                        $item->quality++;
                        if (($item->sellIn < 11) && ($item->quality < 50)) {
                            $item->quality++;
                        }
                        if (($item->sellIn < 6) && ($item->quality < 50)) {
                            $item->quality++;
                        }
                    }
                    $item->sellIn--;

                    // If we are past the concert date, set the quality to 0.
                    if ($item->sellIn < 0) {
                        $item->quality = 0;
                    }

                    break;
                case 'Sulfuras, Hand of Ragnaros':
                    $item->quality = 80;
                    break;
                case 'conjured':
                    $item->sellIn--;
                    if($item->quality > 0) {
                        $item->quality--;
                        if($item->quality > 0) {
                            $item->quality--;
                        }
                    }
                    break;
                default: // Normal Items
                    $item->sellIn--;
                    if ($item->quality > 0) {
                        $item->quality--;
                        if (($item->sellIn < 0) && ($item->quality > 0)) {
                            $item->quality--;
                        }
                    }
                    break;
            } // END switch
        }
    }
}
