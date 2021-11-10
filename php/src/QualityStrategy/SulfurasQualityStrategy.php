<?php

namespace GildedRose\QualityStrategy;

use GildedRose\Item;

class SulfurasQualityStrategy extends BasicQualityStrategy
{
    public function __construct(int $decreasingSpeed = 0)
    {
        parent::__construct($decreasingSpeed);
    }

    protected function setMinAndMaxQuality(Item $item): Item
    {
        return $item;
    }
}
