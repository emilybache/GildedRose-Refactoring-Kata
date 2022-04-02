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
                            $this->increaseQuality($item);
                        }
                        if ($item->sellIn < 6) {
                            $this->increaseQuality($item);
                        }
                    }
                }
            } else {
                $this->decreaseQuality($item);
            }

            $item->sellIn = $item->sellIn - 1;

            if ($item->sellIn < 0) {
                if ($item->name === 'Aged Brie') {
                    $this->increaseQuality($item);
                } else {
                    if ($item->name === 'Backstage passes to a TAFKAL80ETC concert') {
                        $item->quality = $item->quality - $item->quality;
                    } else {
                        $this->decreaseQuality($item);
                    }
                }
            }
        }
    }

    private function increaseQuality(Item &$item): void
    {
        if ($item->quality >= 50) {
            return;
        }
        $item->quality += 1;
    }

    private function decreaseQuality(Item &$item): void
    {
        if ($item->quality > 0) {
            $item->quality -= 1;
        }
    }
}
