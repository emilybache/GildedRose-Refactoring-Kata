<?php

namespace GildedRose\SellInStrategy;

use GildedRose\Item;

class BasicSellInStrategy implements SellInStrategyInterface
{
    public function updateSellIn(Item $item): Item
    {
        $item->sell_in--;

        return $item;
    }
}
