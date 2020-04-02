<?php

namespace App;

interface GildedRoseInterface {

    public function checkQualityExceptSulfurasWhereSellInZero(Item $item);

    public function checkQualityExeceptSulfurasForBrieAndPassProducts(Item $item);

    public function checkQualityLessThanZeroExceptSulfuras(Item $item);

    public function reduceSellInExceptSulfuras(Item $item);


}
