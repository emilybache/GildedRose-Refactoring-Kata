<?php

declare(strict_types=1);

namespace GildedRose\Item;

use GildedRose\Item;

final class NormalItem extends Item
{
    public function update()
    {
        $this->decreaseQuality();
        $this->sellIn = $this->sellIn - 1;
        if ($this->sellIn < 0) {
            $this->decreaseQuality();
        }
    }
}