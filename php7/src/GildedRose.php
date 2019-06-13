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
        $conjuredName = 'Conjured Mana Cake';

        foreach ($this->items as $item) {
            switch ($item->name) {
                case $conjuredName:
                    $conjured = new Conjured($item);
                    $conjured->updateQuality();
                    break;

                case $agedBrieName:
                    $agedBried = new AgedBrie($item);
                    $agedBried->updateQuality();
                    break;

                case $backstageName:
                    $backstagePasses = new BackstagePasses($item);
                    $backstagePasses->updateQuality();
                    break;

                case $sulfarasName:
                    $sulfaras = new Sulfaras($item);
                    $sulfaras->updateQuality();
                    break;

                default:
                    $defoultItem = new DefaultItem($item);
                    $defoultItem->updateQuality();
                    break;
            }
        }
    }
}

