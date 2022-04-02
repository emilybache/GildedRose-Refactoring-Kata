<?php

declare(strict_types=1);

namespace GildedRose;
abstract class Item
{
    public string $name;

    public int $sellIn;

    public int $quality;

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

    abstract public function update();

    protected function increaseQuality(): void
    {
        if ($this->quality >= 50) {
            return;
        }
        $this->quality += 1;
    }

    protected function decreaseQuality(): void
    {
        if ($this->quality <= 0) {
            return;
        }
        $this->quality -= 1;
    }
}
