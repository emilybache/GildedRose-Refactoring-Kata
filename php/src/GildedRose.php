<?php

declare(strict_types=1);

namespace GildedRose;

final class GildedRose
{
    /**
     * @var Item[]
     */
    private array $items;

    private GildedRoseItemFactory $gildedRoseItemFactory;

    /**
     * @param Item[] $items
     */
    public function __construct(array $items)
    {
        $this->items = $items;
        $this->gildedRoseItemFactory = new GildedRoseItemFactory();
    }

    public function updateQuality(): void
    {
        foreach ($this->items as $item) {
            $gildedRoseItem = $this->gildedRoseItemFactory->createGildedRoseItem($item);
            $gildedRoseItem->ageByOneDay();
        }
    }
}
