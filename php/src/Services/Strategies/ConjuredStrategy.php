<?php

namespace GildedRose\Services\Strategies;

use GildedRose\Item;

class ConjuredStrategy extends DefaultStrategy
{
    public function setSellIn(): Item
    {
        $this->item->sell_in--;
        return $this->item;
    }

    public function setQuality(): Item
    {
        $this->item->quality = ($this->item->quality - ($this->degradeRate * 2)) < self::MIN_QUALITY_VALUE ?
            self::MIN_QUALITY_VALUE : $this->item->quality -= ($this->degradeRate * 2);
        return $this->item;
    }
}
