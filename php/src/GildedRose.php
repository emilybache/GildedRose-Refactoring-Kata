<?php

declare(strict_types=1);

namespace GildedRose;

use GildedRose\ItemUpdater\Interface\ItemUpdaterInterface;
use GildedRose\ItemUpdater\AgedBrieItemUpdater;
use GildedRose\ItemUpdater\BackstagePassItemUpdater;
use GildedRose\ItemUpdater\ConjuredItemUpdater;
use GildedRose\ItemUpdater\NormalItemUpdater;
use GildedRose\ItemUpdater\SulfurasItemUpdater;

final class GildedRose
{
    /**
     * @param Item[] $items
     */
    public function __construct(
        private array $items
    ) {
    }

    /**
     * @return void
     */
    public function updateQuality(): void
    {
        foreach ($this->items as $item) {
            $itemUpdater = $this->getItemUpdater($item);
            $itemUpdater->updateSellIn($item);
            $itemUpdater->updateQuality($item);
        }
    }

    /**
     * @param Item $item
     *
     * @return ItemUpdaterInterface
     */
    private function getItemUpdater(Item $item): ItemUpdaterInterface
    {
        return match(true) {
            strpos($item->name, 'Aged Brie') !== false => new AgedBrieItemUpdater(),
            strpos($item->name, 'Backstage') !== false => new BackstagePassItemUpdater(),
            strpos($item->name, 'Sulfuras') !== false => new SulfurasItemUpdater(),
            strpos($item->name, 'Conjured') !== false => new ConjuredItemUpdater(),
            default => new NormalItemUpdater()
        };
    }
}
