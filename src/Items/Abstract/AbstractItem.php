<?php

declare(strict_types=1);

namespace GildedRose\Items\Abstract;

use GildedRose\Item;
use GildedRose\Items\Interface\ItemInterface;

abstract class AbstractItem implements ItemInterface
{
    private const MIN_QUALITY = 0;

    private const MAX_QUALITY = 50;

    abstract public function updateQuality(Item $item): void;

    public function updateSellIn(Item $item): void
    {
        $this->decreaseSellIn($item);
    }

    /**
     * @return integer
     */
    protected function getQualityDecrease(Item $item): int
    {
        return $item->sellIn < 0 ? 2 : 1;
    }

    protected function decreaseSellIn(Item $item): void
    {
        $item->sellIn--;
    }

    /**
     * @param integer $amount
     */
    protected function decreaseQuality(Item $item, int $amount = 1): void
    {
        $item->quality = max(self::MIN_QUALITY, $item->quality - $amount);
    }

    /**
     * @param integer $amount
     */
    protected function increaseQuality(Item $item, int $amount = 1): void
    {
        $item->quality = min(self::MAX_QUALITY, $item->quality + $amount);
    }
}
