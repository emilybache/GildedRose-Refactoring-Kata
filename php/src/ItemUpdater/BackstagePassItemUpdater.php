<?php

declare(strict_types=1);

namespace GildedRose\ItemUpdater;

use GildedRose\Item;
use GildedRose\ItemUpdater\Abstract\AbstractItemUpdater;
use GildedRose\ItemUpdater\Interface\ItemUpdaterInterface;

class BackstagePassItemUpdater extends AbstractItemUpdater implements ItemUpdaterInterface
{
    /**
     * @param Item $item
     *
     * @return void
     */
    public function updateQuality(Item $item): void
    {
        match(true) {
            $item->sellIn <= 0 => $this->decreaseQuality($item, $item->quality),
            $item->sellIn <= 5 => $this->increaseQuality($item, 3),
            $item->sellIn <= 10 => $this->increaseQuality($item, 2),
            default => $this->increaseQuality($item)
        };
    }
}
