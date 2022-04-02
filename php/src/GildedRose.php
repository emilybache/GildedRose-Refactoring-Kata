<?php

declare(strict_types=1);

namespace GildedRose;

final class GildedRose
{
    /**
     * @var Item[]
     */
    private array $items;

    public function __construct(array $items)
    {
        $this->items = $items;
    }

    public function getItems(): array
    {
        return $this->items;
    }

    public function updateQuality(): void
    {
        foreach ($this->items as $item) {
            if ($item->name === 'Sulfuras, Hand of Ragnaros') {
                continue;
            }

            if ($item->name === 'Aged Brie' || $item->name === 'Backstage passes to a TAFKAL80ETC concert') {
                if ($item->quality < 50) {
                    $item->quality = $item->quality + 1;
                    if ($item->name === 'Backstage passes to a TAFKAL80ETC concert') {
                        if ($item->sellIn < 11) {
                            $item->increaseQuality();
                        }
                        if ($item->sellIn < 6) {
                            $item->increaseQuality();
                        }
                    }
                }
            } else {
                $item->decreaseQuality();
            }

            $item->sellIn = $item->sellIn - 1;

            if ($item->sellIn < 0) {
                if ($item->name === 'Aged Brie') {
                    $item->increaseQuality();
                } else {
                    if ($item->name === 'Backstage passes to a TAFKAL80ETC concert') {
                        $item->quality = $item->quality - $item->quality;
                    } else {
                        $item->decreaseQuality();
                    }
                }
            }
        }
    }
}
