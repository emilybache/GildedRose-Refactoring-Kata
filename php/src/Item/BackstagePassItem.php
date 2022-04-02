<?php

declare(strict_types=1);

namespace GildedRose\Item;

use GildedRose\Item;

final class BackstagePassItem extends Item
{
    public function update()
    {
        $this->sellIn -= 1;
        $this->increaseQuality();
        if ($this->sellIn < 10) {
            $this->increaseQuality();
        }
        if ($this->sellIn < 5) {
            $this->increaseQuality();
        }
        if ($this->sellIn < 0) {
            $this->quality = 0;
        }
        
    }
}