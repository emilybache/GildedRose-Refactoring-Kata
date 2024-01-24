<?php

namespace BehatTests;

use Behat\Behat\Tester\Exception\PendingException;
use Behat\Behat\Context\Context;
use Behat\Step\Given;
use Behat\Step\Then;
use Behat\Step\When;
use GildedRose\Item;
use GildedRose\GildedRose;
use PHPUnit\Framework\Assert;

class ServiceLevelContext implements Context
{
    private Item $item;
    private GildedRose $gildedRose;

    #[Then('I should see :expectedOutput')]
    public function iShouldSee(string $expectedOutput): void
    {
        Assert::assertEquals([$expectedOutput], $this->result);
    }

    #[Given('an item with a sell_in of :initialellIn and a quality of :initialQuality')]
    public function anItemWithASellInOfAndAQualityOf(int $initialellIn, int $initialQuality)
    {
        $this->item = new Item('foo', $initialellIn, $initialQuality);
        $this->gildedRose = new GildedRose([$this->item]);
    }

    #[When('I update the quality')]
    public function iUpdateTheQuality()
    {
        $this->gildedRose->updateQuality();
    }

    #[Then('the item should have a quality of :expectedQuality')]
    public function theItemShouldHaveAQualityOf($expectedQuality)
    {
        Assert::assertEquals($expectedQuality, $this->item->quality);
    }

    #[When('I update the quality :noOfDays times')]
    public function iUpdateTheQualityTimes(int $noOfDays)
    {
        for ($i = 0; $i < $noOfDays; $i++) {
            $this->gildedRose->updateQuality();
        }
    }
}
