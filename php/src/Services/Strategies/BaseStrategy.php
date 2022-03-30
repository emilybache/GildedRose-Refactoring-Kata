<?php

namespace GildedRose\Services\Strategies;

use GildedRose\Item;

interface BaseStrategy
{
    public const MAX_QUALITY_VALUE = 50;

    public const MIN_QUALITY_VALUE = 0;

    public function __construct(Item $item);

    public function setSellIn(): Item;

    public function setQuality(): Item;

    public function handle();
}
