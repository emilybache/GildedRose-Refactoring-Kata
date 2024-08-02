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
            $this->updateItemQuality($item);
        }
    }

    private function updateItemQuality($item): void
    {
        if ($item->name == 'Sulfuras, Hand of Ragnaros') {
            return; // Legendary item, no changes needed
        }

        $this->decreaseSellIn($item);

        switch ($item->name) {
            case 'Aged Brie':
                $this->updateAgedBrie($item);
                break;
            case 'Backstage passes to a TAFKAL80ETC concert':
                $this->updateBackstagePasses($item);
                break;
            case 'Conjured Mana Cake':
                $this->updateConjured($item);
                break;
            default:
                $this->updateNormalItem($item);
        }

        $this->ensureQualityInRange($item);
    }
    private function decreaseSellIn($item): void
    {
        $item->sellIn--;
    }

    private function updateAgedBrie($item): void
    {
        $this->increaseQuality($item);
        if ($item->sellIn < 0) {
            $this->increaseQuality($item);
        }
    }

    private function updateBackstagePasses($item): void
    {
        $this->increaseQuality($item);
        if ($item->sellIn < 10) {
            $this->increaseQuality($item);
        }
        if ($item->sellIn < 5) {
            $this->increaseQuality($item);
        }
        if ($item->sellIn < 0) {
            $item->quality = 0;
        }
    }

    private function updateConjured($item): void
    {
        $this->decreaseQuality($item);
        $this->decreaseQuality($item);
        if ($item->sellIn < 0) {
            $this->decreaseQuality($item);
            $this->decreaseQuality($item);
        }
    }

    private function updateNormalItem($item): void
    {
        $this->decreaseQuality($item);
        if ($item->sellIn < 0) {
            $this->decreaseQuality($item);
        }
    }

    private function increaseQuality($item): void
    {
        if ($item->quality < 50) {
            $item->quality++;
        }
    }

    private function decreaseQuality($item): void
    {
        if ($item->quality > 0) {
            $item->quality--;
        }
    }

    private function ensureQualityInRange($item): void
    {
        $item->quality = max(0, min(50, $item->quality));
    }
}
