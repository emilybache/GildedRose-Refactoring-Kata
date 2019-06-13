<?php


namespace App;


class Sulfaras extends GildedRose
{

    private $item;

    public function __construct($item) {
        $this->item = $item;
    }

    public function updateQuality(){
        $this->item->quality = 80;
    }
}