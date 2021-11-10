<?php

namespace GildedRose\SellInStrategy;

use GildedRose\Item;

interface SellInStrategyInterface
{
    public function updateSellIn(Item $item): Item;
}
