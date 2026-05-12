from gilded_rose import GildedRose, Item

class TestGildedRose:
    # --- NORMAL ITEMS ---
    def test_normal_item_quality_decreases_by_1_before_sell_date(self):
        items = [Item("Normal Item", sell_in=10, quality=20)]
        GildedRose(items).update_quality()
        assert items[0].quality == 19
        assert items[0].sell_in == 9

    def test_normal_item_quality_decreases_by_2_after_sell_date(self):
        items = [Item("Normal Item", sell_in=0, quality=20)]
        GildedRose(items).update_quality()
        assert items[0].quality == 18

    # --- AGED BRIE ---
    def test_aged_brie_increases_quality_over_time(self):
        items = [Item("Aged Brie", sell_in=5, quality=10)]
        GildedRose(items).update_quality()
        assert items[0].quality == 11

    def test_aged_brie_increases_quality_double_after_sell_in(self):
        items = [Item("Aged Brie", sell_in=0, quality=10)]
        GildedRose(items).update_quality()
        assert items[0].quality == 12

    # --- SULFURAS ---
    def test_sulfuras_never_changes(self):
        items = [Item("Sulfuras, Hand of Ragnaros", sell_in=5, quality=80)]
        GildedRose(items).update_quality()
        assert items[0].quality == 80
        assert items[0].sell_in == 5

    def test_sulfuras_remains_80_after_sell_in(self):
        items = [Item("Sulfuras, Hand of Ragnaros", sell_in=-1, quality=80)]
        GildedRose(items).update_quality()
        assert items[0].quality == 80

    # --- BACKSTAGE PASSES ---
    def test_backstage_pass_increases_by_2_when_10_days_or_less(self):
        items = [Item("Backstage passes to a TAFKAL80ETC concert", sell_in=10, quality=20)]
        GildedRose(items).update_quality()
        assert items[0].quality == 22

    def test_backstage_pass_increases_by_3_when_5_days_or_less(self):
        items = [Item("Backstage passes to a TAFKAL80ETC concert", sell_in=5, quality=20)]
        GildedRose(items).update_quality()
        assert items[0].quality == 23

    def test_backstage_pass_quality_drops_to_zero_after_concert(self):
        items = [Item("Backstage passes to a TAFKAL80ETC concert", sell_in=0, quality=20)]
        GildedRose(items).update_quality()
        assert items[0].quality == 0

    # --- LÍMITES (EDGE CASES) ---
    def test_quality_never_goes_below_zero(self):
        items = [Item("Normal Item", sell_in=5, quality=0)]
        GildedRose(items).update_quality()
        assert items[0].quality == 0

    def test_quality_never_exceeds_50(self):
        items = [Item("Aged Brie", sell_in=5, quality=50)]
        GildedRose(items).update_quality()
        assert items[0].quality == 50