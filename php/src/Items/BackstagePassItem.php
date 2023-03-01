<?php

declare(strict_types=1);

namespace GildedRose\Items;

use GildedRose\Item;
use GildedRose\Items\Abstract\AbstractItem;
use GildedRose\Items\Interface\ItemInterface;

class BackstagePassItem extends AbstractItem implements ItemInterface
{
    /**
     * @param Item $item
     *
     * @return void
     */
    public function updateQuality(Item $item): void
    {
        match(true) {
            $item->sellIn <= 0 => $this->decreaseQuality($item, $item->quality),
            $item->sellIn <= 5 => $this->increaseQuality($item, 3),
            $item->sellIn <= 10 => $this->increaseQuality($item, 2),
            default => $this->increaseQuality($item)
        };
    }
}
