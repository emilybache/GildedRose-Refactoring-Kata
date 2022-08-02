<?php

declare(strict_types=1);

namespace Tests;

use ApprovalTests\Approvals;
use GildedRose\GildedRose;
use GildedRose\Item;
use PHPUnit\Framework\TestCase;

class GildedRoseTest extends TestCase
{
    public function testFoo(): void
    {
        $items = [new Item('foo', 0, 0)];
        $gildedRose = new GildedRose($items);
        $gildedRose->updateQuality();
        $this->assertSame('fixme', $items[0]->name);
    }

    public function testApproveArray()
    {
        $list = ['zero', 'oxe', 'two', 'three', 'four', 'five'];
        Approvals::approveList($list);
    }

    public function testApproveMap()
    {
        $list = [
            'zero' => 'Lance',
            'one' => 'Jam',
            'two' => 'James',
            'three' => 'LLewellyn',
            'four' => 'Asaph',
            'five' => 'Dana'
        ];
        Approvals::approveList($list);
    }

    public function testApproveString()
    {
        $fudge = 'fadge';
        Approvals::approveString($fudge);

    }
}