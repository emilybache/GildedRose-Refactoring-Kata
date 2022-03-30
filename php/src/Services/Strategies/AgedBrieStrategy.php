<?php

namespace GildedRose\Services\Strategies;

use GildedRose\Item;

class AgedBrieStrategy extends DefaultStrategy
{
    public function setSellIn(): Item
    {
        $this->item->sell_in--;
        return $this->item;
    }

    public function setQuality(): Item
    {
        $this->item->quality = ($this->item->quality + $this->degradeRate) > self::MAX_QUALITY_VALUE ?
            self::MAX_QUALITY_VALUE : $this->item->quality += $this->degradeRate;
        return $this->item;
    }
}
