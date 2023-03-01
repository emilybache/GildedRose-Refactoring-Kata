<?php

declare(strict_types=1);

namespace GildedRose\Items\Interface;

use GildedRose\Item;

interface ItemInterface
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
