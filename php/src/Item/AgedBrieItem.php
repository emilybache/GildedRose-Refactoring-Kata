<?php

declare(strict_types=1);

namespace GildedRose\Item;

use GildedRose\Item;

final class AgedBrieItem extends Item
{
    public function update()
    {
        $this->increaseQuality();
        $this->sellIn -= 1;
        if ($this->sellIn < 0) {
            $this->increaseQuality();
        }
    }
}