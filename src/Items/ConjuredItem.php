<?php

declare(strict_types=1);

namespace GildedRose\Items;

use GildedRose\Item;
use GildedRose\Items\Abstract\AbstractItem;
use GildedRose\Items\Interface\ItemInterface;

class ConjuredItem extends AbstractItem implements ItemInterface
{
    public function updateQuality(Item $item): void
    {
        // "Conjured" items degrade in Quality twice as fast as normal items
        $this->decreaseQuality($item, $this->getQualityDecrease($item) * 2);
    }
}
