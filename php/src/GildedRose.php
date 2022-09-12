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
            if ($item->name != self::AGED_BRIE && $item->name != self::BACKSTAGE_PASSES_TO_A_TAFKAL_80_ETC_CONCERT) {
                if ($item->quality > 0) {
                    if ($item->name != self::SULFURAS_HAND_OF_RAGNAROS) {
                        $item->quality -= 1;
                    }
                }
            } else {
                if ($item->quality < 50) {
                    $item->quality += 1;
                    if ($item->name == self::BACKSTAGE_PASSES_TO_A_TAFKAL_80_ETC_CONCERT) {
                        if ($item->sell_in < 11) {
                            if ($item->quality < 50) {
                                $item->quality += 1;
                            }
                        }
                        if ($item->sell_in < 6) {
                            if ($item->quality < 50) {
                                $item->quality += 1;
                            }
                        }
                    }
                }
            }

            if ($item->name != self::SULFURAS_HAND_OF_RAGNAROS) {
                $item->sell_in -= 1;
            }

            if ($this->isSellDatePassed($item)) {
                if ($item->name != self::AGED_BRIE) {
                    if ($item->name != self::BACKSTAGE_PASSES_TO_A_TAFKAL_80_ETC_CONCERT) {
                        if ($item->quality > 0) {
                            if ($item->name != self::SULFURAS_HAND_OF_RAGNAROS) {
                                $item->quality -= 1;
                            }
                        }
                    } else {
                        $item->quality = 0;
                    }
                } else {
                    if ($item->quality < 50) {
                        $item->quality += 1;
                    }
                }
            }
        }
    }

    private function isSellDatePassed($item): bool
    {
        return $item->sell_in < 0;
    }
}
