<?php

namespace GildedRose\QualityStrategy;

use GildedRose\Item;

class BackstagePassesQualityStrategy extends BasicQualityStrategy
{
    protected const SELL_IN_LAST_5_DAYS = 5;

    protected const SELL_IN_LAST_10_DAYS = 10;

    protected const DECREASING_SPEED_AT_LAST_5_DAYS = -3;

    protected const DECREASING_SPEED_AT_LAST_10_DAYS = -2;

    public function __construct(int $decreasingSpeed = 1)
    {
        parent::__construct($decreasingSpeed * -1);
    }

    protected function getCurrentDecreasingSpeed(Item $item): int
    {
        $decreasingSpeed = parent::getCurrentDecreasingSpeed($item);

        if ($item->sell_in <= self::SELL_IN_LAST_5_DAYS) {
            $decreasingSpeed = self::DECREASING_SPEED_AT_LAST_5_DAYS;
        } elseif ($item->sell_in <= self::SELL_IN_LAST_10_DAYS) {
            $decreasingSpeed = self::DECREASING_SPEED_AT_LAST_10_DAYS;
        }

        return $decreasingSpeed;
    }

    protected function setMinAndMaxQuality(Item $item): Item
    {
        if ($item->sell_in < 0) {
            $item->quality = 0;

            return $item;
        }

        return parent::setMinAndMaxQuality($item);
    }
}
