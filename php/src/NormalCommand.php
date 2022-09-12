<?php

declare(strict_types=1);

namespace GildedRose;

final class NormalCommand implements Command
{
    public function execute(Item $item): void
    {
        $item->decreaseSellDate();
        $item->decreaseQuality();
        if ($item->isSellDatePassed()) {
            $item->decreaseQuality();
        }
    }
}