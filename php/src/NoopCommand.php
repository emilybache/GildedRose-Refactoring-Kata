<?php

declare(strict_types=1);

namespace GildedRose;

final class NoopCommand implements Command
{
    public function execute(Item $item): void
    {
        // noop
    }
}