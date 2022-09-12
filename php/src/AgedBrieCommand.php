<?php

declare(strict_types=1);

namespace GildedRose;

final class AgedBrieCommand implements Command
{
    public function execute(Item $item): void
    {
        $item->decreaseSellDate();

        $item->increaseQuality();
        if ($item->isSellDatePassed()) {
            $item->increaseQuality();
        }
    }
}