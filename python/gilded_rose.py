# -*- coding: utf-8 -*-


class GildedRose(object):
    """Manages the inventory of items at the Gilded Rose inn."""
    
    # Item type constants
    AGED_BRIE = "Aged Brie"
    BACKSTAGE_PASSES = "Backstage passes to a TAFKAL80ETC concert"
    SULFURAS = "Sulfuras, Hand of Ragnaros"
    CONJURED_PREFIX = "Conjured"
    
    # Quality constraints
    MAX_QUALITY = 50
    MIN_QUALITY = 0
    SULFURAS_QUALITY = 80

    def __init__(self, items):
        self.items = items

    def update_quality(self):
        """Updates the quality and sell_in values for all items."""
        for item in self.items:
            self._update_item(item)

    def _update_item(self, item):
        """Updates a single item's quality and sell_in values."""
        if self._is_sulfuras(item):
            # Sulfuras never changes
            return
        
        # Update sell_in date
        item.sell_in -= 1
        
        # Update quality based on item type
        if self._is_aged_brie(item):
            self._update_aged_brie(item)
        elif self._is_backstage_passes(item):
            self._update_backstage_passes(item)
        elif self._is_conjured(item):
            self._update_conjured_item(item)
        else:
            self._update_normal_item(item)

    def _is_sulfuras(self, item):
        """Check if item is Sulfuras."""
        return item.name == self.SULFURAS

    def _is_aged_brie(self, item):
        """Check if item is Aged Brie."""
        return item.name == self.AGED_BRIE

    def _is_backstage_passes(self, item):
        """Check if item is Backstage passes."""
        return item.name == self.BACKSTAGE_PASSES

    def _is_conjured(self, item):
        """Check if item is a Conjured item."""
        return item.name.startswith(self.CONJURED_PREFIX)

    def _update_normal_item(self, item):
        """Update quality for normal items."""
        degradation_rate = self._get_degradation_rate(item)
        self._decrease_quality(item, degradation_rate)

    def _update_conjured_item(self, item):
        """Update quality for Conjured items (degrade twice as fast)."""
        degradation_rate = self._get_degradation_rate(item) * 2
        self._decrease_quality(item, degradation_rate)

    def _update_aged_brie(self, item):
        """Update quality for Aged Brie (increases with age)."""
        self._increase_quality(item)
        if self._is_past_sell_by_date(item):
            self._increase_quality(item)

    def _update_backstage_passes(self, item):
        """Update quality for Backstage passes."""
        if self._is_past_sell_by_date(item):
            # Quality drops to 0 after the concert
            item.quality = 0
        else:
            self._increase_quality(item)
            if item.sell_in < 10:
                self._increase_quality(item)
            if item.sell_in < 5:
                self._increase_quality(item)

    def _get_degradation_rate(self, item):
        """Get the degradation rate based on sell_in date."""
        if self._is_past_sell_by_date(item):
            return 2  # Degrades twice as fast after sell by date
        return 1  # Normal degradation rate

    def _is_past_sell_by_date(self, item):
        """Check if item is past its sell by date."""
        return item.sell_in < 0

    def _increase_quality(self, item):
        """Increase item quality by 1, respecting maximum quality cap."""
        if item.quality < self.MAX_QUALITY:
            item.quality += 1

    def _decrease_quality(self, item, amount):
        """Decrease item quality by specified amount, respecting minimum quality."""
        item.quality = max(self.MIN_QUALITY, item.quality - amount)


class Item:
    """Represents an item in the inventory."""
    
    def __init__(self, name, sell_in, quality):
        self.name = name
        self.sell_in = sell_in
        self.quality = quality

    def __repr__(self):
        return "%s, %s, %s" % (self.name, self.sell_in, self.quality)
