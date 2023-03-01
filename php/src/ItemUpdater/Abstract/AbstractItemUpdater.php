<?php

declare(strict_types=1);

namespace GildedRose\ItemUpdater\Abstract;

use GildedRose\Item;
use GildedRose\ItemUpdater\Interface\ItemUpdaterInterface;

abstract class AbstractItemUpdater implements ItemUpdaterInterface
{
    private const MIN_QUALITY = 0;
    private const MAX_QUALITY = 50;

    /**
     * @param Item $item
     *
     * @return void
     */
    abstract public function updateQuality(Item $item): void;

    /**
     * @param Item $item
     *
     * @return void
     */
    public function updateSellIn(Item $item): void
    {
        $this->decreaseSellIn($item);
    }
    
    /**
     * @param Item $item
     *
     * @return integer
     */
    protected function getQualityDecrease(Item $item): int
    {
        return $item->sellIn < 0 ? 2 : 1;
    }

    /**
     * @param Item $item
     *
     * @return void
     */
    protected function decreaseSellIn(Item $item): void
    {
        $item->sellIn--;
    }

    /**
     * @param Item    $item
     * @param integer $amount
     *
     * @return void
     */
    protected function decreaseQuality(Item $item, int $amount = 1): void
    {
        $item->quality = max(self::MIN_QUALITY, $item->quality - $amount);
    }

    /**
     * @param Item    $item
     * @param integer $amount
     *
     * @return void
     */
    protected function increaseQuality(Item $item, int $amount = 1): void
    {
        $item->quality = min(self::MAX_QUALITY, $item->quality + $amount);
    }
}
