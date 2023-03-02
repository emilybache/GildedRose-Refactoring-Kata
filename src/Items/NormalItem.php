<?php

declare(strict_types=1);

namespace GildedRose\Items;

use GildedRose\Item;
use GildedRose\Items\Abstract\AbstractItem;
use GildedRose\Items\Interface\ItemInterface;

class NormalItem extends AbstractItem implements ItemInterface
{
    public function updateQuality(Item $item): void
    {
        $this->decreaseQuality($item, $this->getQualityDecrease($item));
    }
}
