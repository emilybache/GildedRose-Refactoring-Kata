<?php


namespace App;


class Conjured
{
    private $item;

    public function __construct($item) {
        $this->item = $item;
    }

    public function updateQuality(){
        $this->item->quality -= 2;

        if ($this->item->sell_in <= 0) {
            $this->item->quality -= 2;
        }

        $this->item->sell_in -= 1;
    }
}