<?php

namespace GildedRose\QualityStrategy;

class ConjuredQualityStrategy extends BasicQualityStrategy
{
    public function __construct(int $decreasingSpeed = 1)
    {
        parent::__construct($decreasingSpeed * 2);
    }
}
