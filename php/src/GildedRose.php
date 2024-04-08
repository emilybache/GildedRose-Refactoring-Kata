<?php

declare(strict_types=1);

namespace GildedRose;

use GildedRose\Handlers\AgedBrieItemHandler;
use GildedRose\Handlers\BackStageItemHandler;
use GildedRose\Handlers\ConjuredItemHandler;
use GildedRose\Handlers\DefaultItemHandler;
use GildedRose\Handlers\SulfarusItemHandler;

final class GildedRose
{
    /**
     * @param Item[] $items
     */
    public function __construct(
        private array $items
    ) {
    }

    public function updateQuality(): array
    {
        foreach ($this->items as $item) {
            if ($item->name != 'Aged Brie' and $item->name != 'Backstage passes to a TAFKAL80ETC concert') {
                if ($item->quality > 0) {
                    if ($item->name != 'Sulfuras, Hand of Ragnaros') {
                        $item->quality = $item->quality - 1;
                    }
                }
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

            if ($item->name != 'Sulfuras, Hand of Ragnaros') {
                $item->sellIn = $item->sellIn - 1;
            }

            if ($item->sellIn < 0) {
                if ($item->name != 'Aged Brie') {
                    if ($item->name != 'Backstage passes to a TAFKAL80ETC concert') {
                        if ($item->quality > 0) {
                            if ($item->name != 'Sulfuras, Hand of Ragnaros') {
                                $item->quality = $item->quality - 1;
                            }
                        }
                    } else {
                        $item->quality = $item->quality - $item->quality;
                    }
                } else {
                    if ($item->quality < 50) {
                        $item->quality = $item->quality + 1;
                    }
                }
            }
        }

        return $this->items;
    }

    public function updateQuality1(): array
    {
        foreach ($this->items as $item) {
            $handler = match ($item->name) {
                'Aged Brie' => new AgedBrieItemHandler(),
                'Sulfuras, Hand of Ragnaros' => new SulfarusItemHandler(),
                'Backstage passes to a TAFKAL80ETC concert' => new BackStageItemHandler(),
                'Conjured Mana Cake' => new ConjuredItemHandler(),
                default => new DefaultItemHandler(),
            };

            $handler->handle($item);

            unset($handler);
        }

        return $this->items;
    }
}
