<?php


namespace GildedRose;


class SecondProcessingRule
{
    public function __construct(Item $item)
    {
        $this->item = $item;
    }

    public function secondProcessingRule()
    {
        $item = $this->item;

        if ($item->name != 'Sulfuras, Hand of Ragnaros') {
            $item->sell_in = $item->sell_in - 1;
        }

    }

}