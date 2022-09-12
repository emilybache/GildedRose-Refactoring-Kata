<?php

declare(strict_types=1);

namespace GildedRose;

final class GildedRose
{
    /**
     * @var Item[]
     */
    private $items;
    private CommandFactory $commandFactory;

    public function __construct(array $items)
    {
        $this->items = $items;
        $this->commandFactory = new CommandFactory();
    }

    public function updateQuality(): void
    {
        foreach ($this->items as $item) {
            $command = $this->commandFactory->createFor($item->name);

            $command->execute($item);
        }
    }
}


interface Command
{
    public function execute(Item $item): void;
}

final class CommandFactory
{
    private const SULFURAS_HAND_OF_RAGNAROS = 'Sulfuras, Hand of Ragnaros';
    private const BACKSTAGE_PASSES_TO_A_TAFKAL_80_ETC_CONCERT = 'Backstage passes to a TAFKAL80ETC concert';
    private const AGED_BRIE = 'Aged Brie';

    public function createFor(string $itemName): Command
    {
        if ($itemName === self::AGED_BRIE) {
            return new AgedBrieCommand();
        }

        if ($itemName === self::BACKSTAGE_PASSES_TO_A_TAFKAL_80_ETC_CONCERT) {
            return new BackstageCommand();
        }

        if (in_array($itemName,
                [
                    self::AGED_BRIE,
                    self::BACKSTAGE_PASSES_TO_A_TAFKAL_80_ETC_CONCERT,
                    self::SULFURAS_HAND_OF_RAGNAROS
                ]
            ) === false) {
            return new NormalCommand();
        }

        return new NoopCommand();
    }
}

final class AgedBrieCommand implements Command
{
    public function execute(Item $item): void
    {
        $item->decreaseSellDate();

        $item->increaseQuality();
        if ($item->isSellDatePassed()) {
            $item->increaseQuality();
        }
    }
}

final class BackstageCommand implements Command
{
    public function execute(Item $item): void
    {
        $item->decreaseSellDate();

        if ($item->isSellDatePassed()) {
            $item->quality = 0;
            return;
        }

        $item->increaseQuality();
        if ($item->sell_in <= 10) {
            $item->increaseQuality();
        }

        if ($item->sell_in <= 5) {
            $item->increaseQuality();
        }
    }
}

final class NormalCommand implements Command
{
    public function execute(Item $item): void
    {
        $item->decreaseSellDate();
        $item->decreaseQuality();
        if ($item->isSellDatePassed()) {
            $item->decreaseQuality();
        }
    }
}

final class NoopCommand implements Command
{
    public function execute(Item $item): void
    {
        // noop
    }
}