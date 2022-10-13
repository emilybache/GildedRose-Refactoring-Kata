<?php

declare(strict_types=1);

namespace GildedRose;

final class GildedRose
{
    private const AGED_BRIE = 'Aged Brie';
    private const BACKSTAGE = 'Backstage passes to a TAFKAL80ETC concert';
    private const SHR = 'Sulfuras, Hand of Ragnaros';
    private const CONJURED = 'Conjured Mana Cake';

    private const MAX_THRESHOLD = 50;
    private const MIN_THRESHOLD = 0;

    /**
     * @var Item[]
     */
    private array $items;

    /**
     * @param array $items
     */
    public function __construct(array $items)
    {
        $this->items = $items;
    }

    public function updateQuality(): void
    {
        foreach ($this->items as $item) {
            if (!$this->isLegendaryItem($item)) {
                $item->quality = $this->subtractQuality($item);
            }

            // The quality of conjured items degrade twice as fast as normal items, so subtract once again
            if ($this->isConjuredItem($item)) {
                $item->quality = $this->subtractQuality($item);
            }

            if ($this->qualityIncreases($item)) {
                $item->quality = $this->addQuality($item);

                if ($item->name === self::BACKSTAGE) {
                    if ($item->sell_in <= 10) {
                        $item->quality = $this->addQuality($item);
                    }
                    if ($item->sell_in <= 5) {
                        $item->quality = $this->addQuality($item);
                    }
                }
            }

            if (!$this->isLegendaryItem($item)) {
                $item->sell_in = --$item->sell_in;
            }

            if ($this->sellDateReached($item)) {
                if ($item->name === self::AGED_BRIE) {
                    $item->quality = $this->addQuality($item);
                    continue;
                }

                if ($item->name === self::BACKSTAGE) {
                    // Quality of backstage passes drops to 0 when sell date has been reached
                    $item->quality = 0;
                    continue;
                }

                if (!$this->isLegendaryItem($item)) {
                    $item->quality = $this->subtractQuality($item);
                }
            }
        }
    }

    private function qualityIncreases(Item $item): bool
    {
        return in_array($item->name, [self::AGED_BRIE, self::BACKSTAGE]);
    }

    private function isLegendaryItem(Item $item): bool
    {
        return $item->name === self::SHR;
    }

    private function sellDateReached(Item $item): bool
    {
        return $item->sell_in < 0;
    }

    private function isConjuredItem(Item $item): bool
    {
        return $item->name === self::CONJURED;
    }

    private function subtractQuality(Item $item): int
    {
        $quality = $item->quality;

        return $quality > self::MIN_THRESHOLD ? --$quality : $quality;
    }

    private function addQuality(Item $item): int
    {
        $quality = $item->quality;

        return $quality < self::MAX_THRESHOLD ? ++$quality : $quality;
    }
}
