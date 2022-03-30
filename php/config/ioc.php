<?php

declare(strict_types=1);

use GildedRose\Services\AbstractDegradingStrategyFactory;
use GildedRose\Services\DegradingStrategyFactory;

/**
 * Here We set Bindings between Interfaces and abstraction to implementation
 */
const BINDINGS =
[
    AbstractDegradingStrategyFactory::class => DegradingStrategyFactory::class
];

// Helper Method to get implementation
function resolve(string $interface)
{
    try {
        return BINDINGS[$interface];
    } catch (Throwable $e) {
        throw new Exception('Cannot Find Implementation');
    }
}
