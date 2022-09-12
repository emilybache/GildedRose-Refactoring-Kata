<?php

declare(strict_types=1);

namespace GildedRose;

final class BackstageCommand implements Command
{
    public function execute(Item $item): void
    {
        $item->decreaseSellDate();

        if ($item->isSellDatePassed()) {
            $item->quality = 0;
            return;
        }

        $item->increaseQuality();
        if ($item->sell_in <= 10) {
            $item->increaseQuality();
        }

        if ($item->sell_in <= 5) {
            $item->increaseQuality();
        }
    }
}