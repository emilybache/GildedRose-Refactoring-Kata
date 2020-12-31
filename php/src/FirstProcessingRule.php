<?php


namespace GildedRose;

use GildedRose\Item;

class FirstProcessingRule
{
    private $item;

    public function __construct(Item $item)
    {
        $this->item = $item;
    }

    public function firstProcessingRule()
    {
        $item = $this->item;

        if ($item->name != 'Aged Brie' and $item->name != 'Backstage passes to a TAFKAL80ETC concert') {
            $this->isNotAgedBrieBackstagePasses($item);
        } else {
            $this->isAgedBrieBackstagePasses($item);
        }
    }

    public function isNotAgedBrieBackstagePasses(& $item){
        if ($item->quality > 0) {
            if ($item->name != 'Sulfuras, Hand of Ragnaros') {
                $item->quality = $item->quality - 1;
            }
        }
    }

    public function isAgedBrieBackstagePasses(& $item)
    {
        if ($item->quality < 50) {
            $item->quality = $item->quality + 1;
            if ($item->name == 'Backstage passes to a TAFKAL80ETC concert') {
                if ($item->sell_in < 11) {
                    if ($item->quality < 50) {
                        $item->quality = $item->quality + 1;
                    }
                }
                if ($item->sell_in < 6) {
                    if ($item->quality < 50) {
                        $item->quality = $item->quality + 1;
                    }
                }
            }
        }
    }

}