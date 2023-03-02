<?php

declare(strict_types=1);

namespace GildedRose\Items;

use GildedRose\Item;
use GildedRose\Items\Abstract\AbstractItem;
use GildedRose\Items\Interface\ItemInterface;

class SulfurasItem extends AbstractItem implements ItemInterface
{
    public function updateQuality(Item $item): void
    {
        // Do nothing, Sulfuras never changes in quality
    }
}
