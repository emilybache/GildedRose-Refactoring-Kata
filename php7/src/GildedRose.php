<?php

namespace App;

class GildedRose
{

    private $items = [];

    public function __construct($items)
    {
        $this->items = $items;
    }

    public function updateQuality()
    {
        $agedBrieName = 'Aged Brie';
        $backstageName = 'Backstage passes to a TAFKAL80ETC concert';
        $sulfarasName = 'Sulfuras, Hand of Ragnaros';

        foreach ($this->items as $item) {
            switch ($item->name) {
                case $agedBrieName:
                    $agedBried = new AgedBried($item);
                    $agedBried->updateQuality();
                    return;

                case $backstageName:
                    $backstagePasses = new BackstagePasses($item);
                    $backstagePasses->updateQuality();
                    return;

                case $sulfarasName:
                    $sulfaras = new Sulfaras($item);
                    $sulfaras->updateQuality();
                    return;
                default:
                    $defoultItem = new DefaultItem($item);
                    $defoultItem->updateQuality();
                    return;
            }
        }
    }
}

