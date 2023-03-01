<?php

declare(strict_types=1);

namespace GildedRose\ItemUpdater;

use GildedRose\Item;
use GildedRose\ItemUpdater\Abstract\AbstractItemUpdater;
use GildedRose\ItemUpdater\Interface\ItemUpdaterInterface;

class SulfurasItemUpdater extends AbstractItemUpdater implements ItemUpdaterInterface
{
    /**
     * @param Item $item
     *
     * @return void
     */
    public function updateQuality(Item $item): void
    {
        // Do nothing, Sulfuras never changes in quality
    }
}
