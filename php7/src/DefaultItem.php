<?php


namespace App;


class DefaultItem extends GildedRose
{
    private $item;

    public function __construct($item) {
        $this->item = $item;
    }

    public function updateQuality(){
        $this->item->quality -= 1;

        if ($this->item->sell_in <= 0) {
            $this->item->quality -= 1;
        }

        if($this->item->quality >= 50){
            $this->item->quality = 50;
        }

        $this->item->sell_in -= 1;

    }
}