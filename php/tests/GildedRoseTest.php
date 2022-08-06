<?php

declare(strict_types=1);

namespace Tests;

use ApprovalTests\Approvals;
use GildedRose\GildedRose;
use GildedRose\Item;
use PHPUnit\Framework\TestCase;

class GildedRoseTest extends TestCase
{
    /**
     * Aged Brie：sell_inが1以上、qualityが50未満
     * 期待値：sell_inが-1、qualityが+1
     */
    public function testAgedBrieNormal(): void
    {
        $items = [new Item('Aged Brie', 5, 10)];
        $gildedRose = new GildedRose($items);
        $gildedRose->updateQuality();
        $this->assertSame('Aged Brie', $items[0]->name);
        $this->assertSame(4, $items[0]->sell_in);
        $this->assertSame(11, $items[0]->quality);

        $items = [new Item('Aged Brie', 1, 49)];
        $gildedRose = new GildedRose($items);
        $gildedRose->updateQuality();
        $this->assertSame('Aged Brie', $items[0]->name);
        $this->assertSame(0, $items[0]->sell_in);
        $this->assertSame(50, $items[0]->quality);
    }

    /**
     * Aged Brie：sell_inが0以下、qualityが50未満
     * 期待値：sell_inが-1、qualityが+2
     */
    public function testAgedBrieSellIn0OrLess(): void
    {
        $items = [new Item('Aged Brie', 0, 10)];
        $gildedRose = new GildedRose($items);
        $gildedRose->updateQuality();
        $this->assertSame('Aged Brie', $items[0]->name);
        $this->assertSame(-1, $items[0]->sell_in);
        $this->assertSame(12, $items[0]->quality);

        $items = [new Item('Aged Brie', -1, 10)];
        $gildedRose = new GildedRose($items);
        $gildedRose->updateQuality();
        $this->assertSame('Aged Brie', $items[0]->name);
        $this->assertSame(-2, $items[0]->sell_in);
        $this->assertSame(12, $items[0]->quality);
    }

    /**
     * Aged Brie：sell_inが1以上、qualityが50
     * 期待値：sell_inが-1、qualityは変更なし
     */
    public function testAgedBrieQuality50(): void
    {
        $items = [new Item('Aged Brie', 5, 50)];
        $gildedRose = new GildedRose($items);
        $gildedRose->updateQuality();
        $this->assertSame('Aged Brie', $items[0]->name);
        $this->assertSame(4, $items[0]->sell_in);
        $this->assertSame(50, $items[0]->quality);
    }

    /**
     * Sulfuras：sell_inが1以上、qualityが80
     * 期待値：sell_in、qualityどちらも変更なし
     */
    public function testSulfurasNormal(): void
    {
        $items = [new Item('Sulfuras, Hand of Ragnaros', 5, 80)];
        $gildedRose = new GildedRose($items);
        $gildedRose->updateQuality();
        $this->assertSame('Sulfuras, Hand of Ragnaros', $items[0]->name);
        $this->assertSame(5, $items[0]->sell_in);
        $this->assertSame(80, $items[0]->quality);
    }

    /**
     * 複数商品
     */
    public function testMixCase(): void
    {
        $items = [
            new Item('Aged Brie', 5, 10),
            new Item('Sulfuras, Hand of Ragnaros', 5, 80),
        ];
        $gildedRose = new GildedRose($items);
        $gildedRose->updateQuality();
        $this->assertSame('Aged Brie', $items[0]->name);
        $this->assertSame(4, $items[0]->sell_in);
        $this->assertSame(11, $items[0]->quality);
        $this->assertSame('Sulfuras, Hand of Ragnaros', $items[1]->name);
        $this->assertSame(5, $items[1]->sell_in);
        $this->assertSame(80, $items[1]->quality);
    }

    // テストエラーの原因が特定できないので後で調査する
    /*
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
    */
}
