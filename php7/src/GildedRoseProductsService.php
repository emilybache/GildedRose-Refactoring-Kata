<?php

namespace App;

class GildedRoseProductsService {

    public $GildedRose;
    public function __construct(GildedRoseInterface $gildedRose) {
       $this->GildedRose = $gildedRose;
    }

    public function updateQuality() {

        foreach ($this->GildedRose->getItems() as $item) {
            $this->GildedRose->checkQualityLessThanZeroExceptSulfuras($item);
            $this->GildedRose->checkQualityExeceptSulfurasForBrieAndPassProducts($item);
            $this->GildedRose->reduceSellInExceptSulfuras($item);
            $this->GildedRose->checkQualityExceptSulfurasWhereSellInZero($item);
        }
    }
}