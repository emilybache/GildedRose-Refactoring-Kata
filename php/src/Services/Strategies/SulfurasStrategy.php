<?php

namespace GildedRose\Services\Strategies;

use GildedRose\Item;

class SulfurasStrategy extends DefaultStrategy
{
    public function setSellIn(): Item
    {
        return $this->item;
    }

    public function setQuality(): Item
    {
        return $this->item;
    }
}
