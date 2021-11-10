<?php

namespace GildedRose\QualityStrategy;

use GildedRose\Item;

class BasicQualityStrategy implements QualityStrategyInterface
{
    /**
     * @var int
     */
    protected $decreasingSpeed;

    public function __construct(int $decreasingSpeed = 1)
    {
        $this->decreasingSpeed = $decreasingSpeed;
    }

    public function updateQuality(Item $item): Item
    {
        $item = $this->setInitialQuality($item);

        $decreasingSpeed = $this->getCurrentDecreasingSpeed($item);

        $item->quality -= $decreasingSpeed;

        return $this->setFinalQuality($item);
    }

    protected function getCurrentDecreasingSpeed(Item $item): int
    {
        return $item->sell_in >= 0
            ? $this->decreasingSpeed
            : $this->decreasingSpeed * 2;
    }

    protected function setInitialQuality(Item $item): Item
    {
        return $this->setMinAndMaxQuality($item);
    }

    protected function setFinalQuality(Item $item): Item
    {
        return $this->setMinAndMaxQuality($item);
    }

    protected function setMinAndMaxQuality(Item $item): Item
    {
        $item->quality = $item->quality > 0 ? $item->quality : 0;
        $item->quality = $item->quality < self::QUALITY_MAX_LEVEL ? $item->quality : self::QUALITY_MAX_LEVEL;

        return $item;
    }
}
