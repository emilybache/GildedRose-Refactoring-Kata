<?php

namespace GildedRose\Services\Strategies;

use GildedRose\Item;

class BackstageStrategy extends DefaultStrategy
{
    public function setDegradeRate()
    {
        if ($this->item->sell_in <= 0) {
            $this->degradeRate = -$this->item->quality;
        } elseif ($this->item->sell_in <= 5) {
            $this->degradeRate = 3;
        } elseif ($this->item->sell_in <= 10) {
            $this->degradeRate = 2;
        }
    }

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
