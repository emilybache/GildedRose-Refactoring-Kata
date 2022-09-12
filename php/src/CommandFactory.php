<?php

declare(strict_types=1);

namespace GildedRose;

final class CommandFactory
{
    private const SULFURAS_HAND_OF_RAGNAROS = 'Sulfuras, Hand of Ragnaros';
    private const BACKSTAGE_PASSES_TO_A_TAFKAL_80_ETC_CONCERT = 'Backstage passes to a TAFKAL80ETC concert';
    private const AGED_BRIE = 'Aged Brie';

    public function createFor(string $itemName): Command
    {
        if ($itemName === self::AGED_BRIE) {
            return new AgedBrieCommand();
        }

        if ($itemName === self::BACKSTAGE_PASSES_TO_A_TAFKAL_80_ETC_CONCERT) {
            return new BackstageCommand();
        }

        if (in_array($itemName,
                [
                    self::AGED_BRIE,
                    self::BACKSTAGE_PASSES_TO_A_TAFKAL_80_ETC_CONCERT,
                    self::SULFURAS_HAND_OF_RAGNAROS
                ]
            ) === false) {
            return new NormalCommand();
        }

        return new NoopCommand();
    }
}