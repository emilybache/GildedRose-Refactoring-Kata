<?php

namespace GildedRose\SellInStrategy;

use GildedRose\Item;

class SulfurasSellInStrategy implements SellInStrategyInterface
{
    public function updateSellIn(Item $item): Item
    {
        return $item;
    }
}
