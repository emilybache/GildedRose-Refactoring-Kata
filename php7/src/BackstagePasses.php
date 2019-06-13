<?php


namespace App;


class BackstagePasses extends GildedRose
{
    private $item;

    public function __construct($item) {
        $this->item = $item;
    }

    public function updateQuality(){
        $this->item->quality += 1;

        if ($this->item->sell_in <= 10) {
            $this->item->quality += 1;
        }

        if ($this->item->sell_in <= 5) {
            $this->item->quality += 1;
        }

        if ($this->item->quality > 50) {
            $this->item->quality = 50;
        }

        if ($this->item->sell_in <= 0) {
            $this->item->quality = 0;
        }

        $this->item->sell_in -= 1;
    }
}