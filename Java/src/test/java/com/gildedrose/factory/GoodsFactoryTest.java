package com.gildedrose.factory;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;

import com.gildedrose.AgedBrie;
import com.gildedrose.BackStagePasses;
import com.gildedrose.Generic;
import com.gildedrose.Goods;
import com.gildedrose.GoodsType;
import com.gildedrose.Sulfuras;

class GoodsFactoryTest {

	@Test
	void testGetGoods() {
		// Test AgedBrie
		Goods agedBrie = GoodsFactory.getGoods(GoodsType.AGED_BRIE.getGoodsName());
		assertTrue(agedBrie instanceof AgedBrie);

		// Test BackStagePasses
		Goods backStagePasses = GoodsFactory.getGoods(GoodsType.BACK_STAGE_PASSES.getGoodsName());
		assertTrue(backStagePasses instanceof BackStagePasses);

		// Test Sulfuras
		Goods sulfuras = GoodsFactory.getGoods(GoodsType.SULFURAS.getGoodsName());
		assertTrue(sulfuras instanceof Sulfuras);

		// Test Generic
		Goods generic = GoodsFactory.getGoods("normal item");
		assertTrue(generic instanceof Generic);
	}

}
