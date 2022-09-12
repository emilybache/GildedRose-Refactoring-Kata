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
    public $sell_in;

    /**
     * @var int
     */
    public $quality;

    public function __construct(string $name, int $sell_in, int $quality)
    {
        $this->name = $name;
        $this->sell_in = $sell_in;
        $this->quality = $quality;
    }

    public function __toString(): string
    {
        return "{$this->name}, {$this->sell_in}, {$this->quality}";
    }

    public function increaseQuality(): void
    {
        if ($this->quality < 50) {
            $this->quality += 1;
        }
    }

    public function decreaseQuality(): void
    {
        if ($this->quality > 0) {
            $this->quality -= 1;
        }
    }

    public function isSellDatePassed(): bool
    {
        return $this->sell_in < 0;
    }

    public function decreaseSellDate(): void
    {
        $this->sell_in -= 1;
    }
}
