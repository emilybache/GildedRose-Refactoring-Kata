<?php

declare(strict_types=1);

namespace GildedRose;

final class Item
{
    /**
     * @var string
     */
    public $name;

    /**
     * @var int
     */
    public $sellIn;

    /**
     * @var int
     */
    public $quality;

    public function __construct(string $name, int $sellIn, int $quality)
    {
        $this->name = $name;
        $this->sellIn = $sellIn;
        $this->quality = $quality;
    }

    public function __toString(): string
    {
        return "{$this->name}, {$this->sellIn}, {$this->quality}";
    }
}
