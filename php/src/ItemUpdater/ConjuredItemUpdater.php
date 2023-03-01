<?php

declare(strict_types=1);

namespace GildedRose\ItemUpdater;

use GildedRose\Item;
use GildedRose\ItemUpdater\Abstract\AbstractItemUpdater;
use GildedRose\ItemUpdater\Interface\ItemUpdaterInterface;

class ConjuredItemUpdater extends AbstractItemUpdater implements ItemUpdaterInterface
{
    /**
     * @param Item $item
     *
     * @return void
     */
    public function updateQuality(Item $item): void
    {
        // "Conjured" items degrade in Quality twice as fast as normal items
        $this->decreaseQuality($item, $this->getQualityDecrease($item) * 2);
    }
}
