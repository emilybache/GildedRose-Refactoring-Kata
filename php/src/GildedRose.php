<?php

declare(strict_types=1);

namespace GildedRose;

use GildedRose\Items\Factory\ItemFactory;

final class GildedRose
{
    /**
     * @param Item[] $items
     */
    public function __construct(
        private array $items
    ) {
    }

    public function updateQuality(): void
    {
        foreach ($this->items as $item) {
            $itemInstance = ItemFactory::createItem($item);
            $itemInstance->updateSellIn($item);
            $itemInstance->updateQuality($item);
        }
    }
}
