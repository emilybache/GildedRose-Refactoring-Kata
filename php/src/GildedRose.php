<?php

namespace GildedRose;

final class GildedRose
{
    public function __construct(
        private array $items
    )
    {
    }

    public function updateQuality(): void
    {
        /** @var Item $item */
        foreach ($this->items as $item) {
            if ($item->name === 'Sulfuras, Hand of Ragnaros') {
                continue;
            }

            $disabledNames = ['Aged Brie', 'Backstage passes to a TAFKAL80ETC concert'];
            if (in_array($item->name, $disabledNames, true)) {
                if ($item->quality < 50) {
                    $this->increaseQuality($item, 1);
                    if ($item->name === 'Backstage passes to a TAFKAL80ETC concert') {
                        if ($item->sell_in <= 10 && $item->sell_in > 5) {
                            $this->increaseQuality($item, 1);
                        } elseif ($item->sell_in <= 5) {
                            $this->increaseQuality($item, 2);
                        }
                    }
                }
            } else {
                if ($item->quality > 0) {
                    $this->lowerQuality($item, 1);
                }
                if ($item->name === 'Conjured Mana Cake') {
                    $this->lowerQuality($item, 1);
                }
            }

            if ($item->sell_in <= 0) {
                if ($item->name === 'Conjured Mana Cake') {
                    $this->lowerQuality($item, 1);
                }
                if ($item->name === 'Aged Brie') {
                    if ($item->quality < 50) {
                        $this->increaseQuality($item, 1);
                    }
                } elseif ($item->name === 'Backstage passes to a TAFKAL80ETC concert') {
                    $this->lowerQuality($item, $item->quality);
                } elseif ($item->quality > 0) {
                    $this->lowerQuality($item, 1);
                }
            }
            $this->lowerSellIn($item);
        }
    }

    private function lowerQuality(Item $item, int $amount): void
    {
        if ($item->quality - $amount > 0) {
            $item->quality -= $amount;
        } else {
            $item->quality = 0;
        }
    }

    private function increaseQuality(Item $item, int $amount): void
    {
        if ($item->quality + $amount <= 50) {
            $item->quality += $amount;
        } else {
            $item->quality = 50;
        }
    }

    private function lowerSellIn(Item $item): void
    {
        --$item->sell_in;
    }
}
