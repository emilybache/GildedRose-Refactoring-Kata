<?php

declare(strict_types=1);

namespace GildedRose\ItemUpdater\Interface;

use GildedRose\Item;

interface ItemUpdaterInterface
{
    /**
     * @param Item $item
     *
     * @return void
     */
    public function updateQuality(Item $item): void;

    /**
     * @param Item $item
     *
     * @return void
     */
    public function updateSellIn(Item $item): void;
}
