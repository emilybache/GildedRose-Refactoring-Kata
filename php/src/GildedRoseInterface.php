<?php


namespace GildedRose;

use GildedRose\Item;

interface GildedRoseInterface
{
    public function updateQuality();

    public function firstProcessingRule(Item &$item);

    public function secondProcessingRule(Item &$item);

    public function thirdProcessingRule(Item &$item);

}