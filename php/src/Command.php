<?php

declare(strict_types=1);

namespace GildedRose;

interface Command
{
    public function execute(Item $item): void;
}