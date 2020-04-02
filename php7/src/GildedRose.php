<?php

namespace App;

final class GildedRose implements GildedRoseInterface {

    const AGED_BRIE = 'Aged Brie';
    const BACKSTAGE_PASS = 'Backstage passes to a TAFKAL80ETC concert';
    const SULFULRAS_RAGNAROS = 'Sulfuras, Hand of Ragnaros';
    const CONJURED = 'Conjured Mana Cake';
    const SELL_IN_ZERO = 0;
    const QUALITY_ZERO = 0;
    const QUALITY_MID = 11;
    const QUALITY_LOW = 6;
    const QUALITY_MAX = 50;

    private $items = [];


    public function __construct($items) {
        $this->items = $items;
    }

    public function getItems(){
        return $this->items;
    }

    public function checkQualityExceptSulfurasWhereSellInZero(Item $item)
    {
            if ($item->sell_in < self::SELL_IN_ZERO) {
                if ($item->name != self::AGED_BRIE) {
                    if ($item->name != self::BACKSTAGE_PASS) {
                        if ($item->quality > self::QUALITY_ZERO) {
                            if ($item->name != self::SULFULRAS_RAGNAROS) {
                                $item->quality = $item->quality - 1;
                            }
                        }
                    } else {
                        $item->quality = $item->quality - $item->quality;
                    }
                } else {
                    if ($item->quality < self::QUALITY_MAX) {
                        $item->quality = $item->quality + 1;
                    }
                }
            }

    }

    public function checkQualityExeceptSulfurasForBrieAndPassProducts(Item $item) {
        if (!($item->name != GildedRose::AGED_BRIE and $item->name != GildedRose::BACKSTAGE_PASS)) {
            if ($item->quality < self::QUALITY_MAX) {
                $item->quality = $item->quality + 1;
                if ($item->name == self::BACKSTAGE_PASS) {
                    if ($item->sell_in < self::QUALITY_MID) {
                        $item->quality = $item->quality + 1;
                    }
                    if ($item->sell_in < self::QUALITY_LOW) {
                        $item->quality = $item->quality + 1;
                    }
                }
            }
        }

    }

    public function checkQualityLessThanZeroExceptSulfuras(Item $item){
        if ($item->name != GildedRose::AGED_BRIE and $item->name != GildedRose::BACKSTAGE_PASS) {
            if ($item->quality > self::QUALITY_ZERO) {
                if ($item->name != self::SULFULRAS_RAGNAROS) {
                    if($item->name == self::CONJURED) {
                        $item->quality = ($item->quality == 1) ? $item->quality - 1 : $item->quality - 2;
                    }else {
                        $item->quality = $item->quality - 1;
                    }
                }
            }
        }
    }

    public function reduceSellInExceptSulfuras(Item $item){
        //reduce sellin
        if ($item->name != self::SULFULRAS_RAGNAROS) {
            $item->sell_in = $item->sell_in - 1;
        }
    }

}

