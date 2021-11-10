<?php

namespace GildedRose\QualityStrategy;

use GildedRose\Item;

interface QualityStrategyInterface
{
    public const QUALITY_MAX_LEVEL = 50;

    public function updateQuality(Item $item): Item;
}
