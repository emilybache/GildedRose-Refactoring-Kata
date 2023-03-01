<?php

declare(strict_types=1);

namespace GildedRose\ItemUpdater;

use GildedRose\Item;
use GildedRose\ItemUpdater\Abstract\AbstractItemUpdater;
use GildedRose\ItemUpdater\Interface\ItemUpdaterInterface;

class NormalItemUpdater extends AbstractItemUpdater implements ItemUpdaterInterface
{
    /**
     * @param Item $item
     *
     * @return void
     */
    public function updateQuality(Item $item): void
    {
        $this->decreaseQuality($item, $this->getQualityDecrease($item));
    }
}
