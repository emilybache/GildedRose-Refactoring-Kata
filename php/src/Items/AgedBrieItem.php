<?php

declare(strict_types=1);

namespace GildedRose\Items;

use GildedRose\Item;
use GildedRose\Items\Abstract\AbstractItem;
use GildedRose\Items\Interface\ItemInterface;

class AgedBrieItem extends AbstractItem implements ItemInterface
{
    public function updateQuality(Item $item): void
    {
        // "Aged Brie" actually increases in Quality the older it gets
        $this->increaseQuality($item, $this->getQualityDecrease($item));
    }
}
