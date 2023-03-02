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
        // We don't use symfony, so tempory load the mapping file simple like this.
        // In symfony we can use ParameterBagInterface to load yaml config
        $mappingFile = __DIR__ . '/../config/item_mapping.php';
        $itemFactory = new ItemFactory($mappingFile);

        foreach ($this->items as $item) {
            $itemInstance = $itemFactory->createItem($item);
            $itemInstance->updateSellIn($item);
            $itemInstance->updateQuality($item);
        }
    }
}
