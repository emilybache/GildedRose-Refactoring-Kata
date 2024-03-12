<?php

declare(strict_types=1);

namespace GildedRose\Enum;

enum Operation: string
{
    case INCREASE = 'increase';
    case DECREASE = 'decrease';
    case RESET = 'reset';
}
