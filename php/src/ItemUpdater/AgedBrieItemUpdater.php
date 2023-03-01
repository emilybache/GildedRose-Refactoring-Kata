<?php

declare(strict_types=1);

namespace GildedRose\ItemUpdater;

use GildedRose\Item;
use GildedRose\ItemUpdater\Abstract\AbstractItemUpdater;
use GildedRose\ItemUpdater\Interface\ItemUpdaterInterface;

class AgedBrieItemUpdater extends AbstractItemUpdater implements ItemUpdaterInterface
{
    /**
     * @param Item $item
     *
     * @return void
     */
    public function updateQuality(Item $item): void
    {
        // "Aged Brie" actually increases in Quality the older it gets
        $this->increaseQuality($item, $this->getQualityDecrease($item));
    }
}
