<?php

declare(strict_types=1);

namespace GildedRose\Items\Interface;

use GildedRose\Item;

interface ItemInterface
{
    public function updateQuality(Item $item): void;

    public function updateSellIn(Item $item): void;
}
