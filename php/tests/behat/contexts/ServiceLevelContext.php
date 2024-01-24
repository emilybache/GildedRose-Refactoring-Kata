<?php

declare(strict_types=1);

namespace BehatTests;

use Behat\Behat\Context\Context;
use Behat\Step\Given;
use Behat\Step\Then;
use Behat\Step\When;
use GildedRose\GildedRose;
use GildedRose\Item;
use PHPUnit\Framework\Assert;

class ServiceLevelContext implements Context
{
    private Item $item;

    private GildedRose $gildedRose;

    #[Given('an item with a sell-in of :initialSellIn and a quality of :initialQuality')]
    public function anItemWithASellInOfAndAQualityOf(int $initialSellIn, int $initialQuality): void
    {
        $this->createItem('foo', $initialSellIn, $initialQuality);
    }

    #[Given('an item with a sell-in of :initialSellIn')]
    public function anItemWithASellInOf(int $initialSellIn): void
    {
        $this->createItem('foo', $initialSellIn, 20);
    }

    #[When('I update the quality')]
    #[When('I update the sell-in')]
    public function iUpdateTheQuality(): void
    {
        $this->gildedRose->updateQuality();
    }

    #[Then('the item should have a quality of :expectedQuality')]
    public function theItemShouldHaveAQualityOf($expectedQuality): void
    {
        Assert::assertEquals($expectedQuality, $this->item->quality);
    }

    #[When('I update the quality :noOfDays times')]
    public function iUpdateTheQualityTimes(int $noOfDays): void
    {
        for ($i = 0; $i < $noOfDays; $i++) {
            $this->gildedRose->updateQuality();
        }
    }

    #[Then('the item should have a sell-in of :expectedSellIn')]
    public function theItemShouldHaveASellInOf(int $sellIn): void
    {
        Assert::assertEquals($sellIn, $this->item->sellIn);
    }

    #[Given('some brie with a sell-in of :initialSellIn and a quality of :initialQuality')]
    public function someBrieWithASellInOfAndAQualityOf(int $initialSellIn, int $initialQuality): void
    {
        $this->createItem('Aged Brie', $initialSellIn, $initialQuality);
    }

    #[Given('a backstage pass with a sell-in of :initialSellIn and a quality of :initialQuality')]
    public function aBackstagePassWithASellInOfAndAQualityOf(int $initialSellIn, int $initialQuality): void
    {
        $this->createItem('Backstage passes to a TAFKAL80ETC concert', $initialSellIn, $initialQuality);
    }

    #[Given('a sulfura with a sell-in of :initialSellIn and a quality of :initialQuality')]
    public function aSulfuraWithASellInOfAndAQualityOf(int $initialSellIn, int $initialQuality): void
    {
        $this->createItem('Sulfuras, Hand of Ragnaros', $initialSellIn, $initialQuality);
    }

    #[Given('a conjured item with a sell-in of :initialSellIn and a quality of :initialQuality')]
    public function aConjuredItemWithASellInOfAndAQualityOf(int $initialSellIn, int $initialQuality): void
    {
        $this->createItem('Conjured Mana Cake', $initialSellIn, $initialQuality);
    }

    private function createItem(string $name, int $initialSellIn, int $initialQuality): void
    {
        $this->item = new Item($name, $initialSellIn, $initialQuality);
        $this->gildedRose = new GildedRose([$this->item]);
    }
}
