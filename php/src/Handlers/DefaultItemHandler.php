<?php

namespace GildedRose\Handlers;

use GildedRose\Item;

class DefaultItemHandler implements ItemHandlerInterface
{
    private const KOEF_DECREASE = 2;

    public function handle(Item $item): Item
    {
        $item->sellIn = $this->changeSallIn($item->sellIn);
        $item->quality = $this->changeQuality($item->quality, $item->sellIn);

        return $item;
    }


    private function changeQuality(int $quality, int $sallIn): int
    {
        if ($quality === 0) {
            return 0;
        }

        if ($sallIn === 0) {
            return $quality - self::KOEF_DECREASE;
        }

        return $quality - 1;
    }
    private function changeSallIn(int $sallIn): int
    {
        if ($sallIn === 0) {
            return 0;
        }

        return $sallIn - 1;
    }
}