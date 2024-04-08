<?php

declare(strict_types=1);

namespace Tests;

use GildedRose\GildedRose;
use GildedRose\Item;
use PHPUnit\Framework\Assert;
use PHPUnit\Framework\TestCase;

/**
 * This unit test uses [Approvals](https://github.com/approvals/ApprovalTests.php).
 * 
 * There are two test cases here with different styles:
 * <li>"foo" is more similar to the unit test from the 'Java' version
 * <li>"thirtyDays" is more similar to the TextTest from the 'Java' version
 *
 * I suggest choosing one style to develop and deleting the other.
 */
class DefaultItemTest extends TestCase
{
	public function testDefaultChange(): void
    {
        $items = [new Item('default', 1, 1)];
        $app = new GildedRose($items);
        $actualAItems = $app->updateQuality();

        $itemsExpected = [new Item('default', 0, 0)];

		Assert::assertEquals($itemsExpected, $actualAItems);
	}

    public function testDefaultAfterSallInChange(): void
    {
        $items = [new Item('default', -1, 6)];
        $app = new GildedRose($items);
        $actualAItems = $app->updateQuality();

        $itemsExpected = [new Item('default', -2, 4)];

        Assert::assertEquals($itemsExpected, $actualAItems);
    }

    public function testDefaulWithZeroSallInChange(): void
    {
        $items = [new Item('default', 0, 10)];
        $app = new GildedRose($items);
        $actualAItems = $app->updateQuality();

        $itemsExpected = [new Item('default', -1, 8)];

        Assert::assertEquals($itemsExpected, $actualAItems);
    }
}
