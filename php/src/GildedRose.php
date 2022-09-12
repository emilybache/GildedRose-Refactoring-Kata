<?php

declare(strict_types=1);

namespace GildedRose;

final class GildedRose
{
    private const SULFURAS_HAND_OF_RAGNAROS = 'Sulfuras, Hand of Ragnaros';
    private const BACKSTAGE_PASSES_TO_A_TAFKAL_80_ETC_CONCERT = 'Backstage passes to a TAFKAL80ETC concert';
    private const AGED_BRIE = 'Aged Brie';
    /**
     * @var Item[]
     */
    private $items;

    public function __construct(array $items)
    {
        $this->items = $items;
    }

    public function updateQuality(): void
    {
        foreach ($this->items as $item) {
            if ($item->name !== self::SULFURAS_HAND_OF_RAGNAROS) {
                $item->decreaseSellDate();
            }

            if ($item->name === self::AGED_BRIE) {
                $item->increaseQuality();
                if ($item->isSellDatePassed()) {
                    $item->increaseQuality();
                }
            }

            if ($item->name === self::BACKSTAGE_PASSES_TO_A_TAFKAL_80_ETC_CONCERT) {
                $item->increaseQuality();
                if ($item->sell_in <= 10) {
                    $item->increaseQuality();
                }
                if ($item->sell_in <= 5) {
                    $item->increaseQuality();
                }
            }


            if (in_array($item->name, [self::AGED_BRIE, self::BACKSTAGE_PASSES_TO_A_TAFKAL_80_ETC_CONCERT, self::SULFURAS_HAND_OF_RAGNAROS]) === false) {
                $item->decreaseQuality();
                if ($item->isSellDatePassed()) {
                    $item->decreaseQuality();
                }
            }

            if ($item->isSellDatePassed() && $item->name === self::BACKSTAGE_PASSES_TO_A_TAFKAL_80_ETC_CONCERT) {
                $item->quality = 0;
            }
        }
    }
}
