<?php

declare(strict_types=1);

namespace GildedRose\Items\Factory;

use GildedRose\Item;
use GildedRose\Items\Interface\ItemInterface;
use GildedRose\Items\NormalItem;

class ItemFactory
{
    private array $itemMappings;

    public function __construct(string $configFilePath)
    {
        $this->itemMappings = require $configFilePath;
    }

    public function createItem(Item $item): ItemInterface
    {
        foreach ($this->itemMappings as $itemName => $itemClassName) {
            if (! class_exists($itemClassName)) {
                throw new \RuntimeException(sprintf('Class "%s" does not exist', $itemClassName));
            }

            if (str_contains($item->name, $itemName)) {
                $reflection = new \ReflectionClass($itemClassName);
                $instance = $reflection->newInstance();

                if (! $instance instanceof ItemInterface) {
                    throw new \RuntimeException(sprintf('Class "%s" does not implement ItemInterface', $itemClassName));
                }
                return $instance;
            }
        }
        return new NormalItem();
    }
}
