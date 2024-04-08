<?php

namespace GildedRose\Handlers;

use GildedRose\Item;

interface ItemHandlerInterface
{
    public function handle(Item $item): Item;
}