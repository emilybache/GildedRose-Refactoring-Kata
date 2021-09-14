# -*- coding: utf-8 -*-

"""
Use and apply the logic from Gilded Rose
"""

import constants


def decrease_sell_in(item, amount=1):
    """
    Decrease sell in value by the specified amount
    :param item:
    :param amount:
    :return:
    """
    item.sell_in -= amount


def decrease_quality(item, amount=1):
    """
    Decrease quality value by the specified amount
    :param item:
    :param amount:
    :return:
    """
    item.quality -= amount


def increase_quality(item, amount=1):
    """
    Increase quality value by the specified amount
    :param item:
    :param amount:
    :return:
    """
    item.quality += amount


def common_item(item):
    """Generate common item logic"""
    if item.quality > 0:
        decrease_quality(item)
    decrease_sell_in(item)
    if item.sell_in < 0 < item.quality:
        decrease_quality(item)


def aged_brie(item):
    """
    Generate Aged Brie logic
    :param item:
    :return:
    """
    if item.quality < 50:
        increase_quality(item)
    decrease_sell_in(item)
    if item.sell_in < 0 and item.quality < 50:
        increase_quality(item)


def backstage(item):
    """
    Generate Backstage Passes logic
    :param item:
    :return:
    """
    if item.quality < 50:
        item.quality = item.quality + 1
        if item.sell_in < 11 and item.quality < 50:
            increase_quality(item)
        if item.sell_in < 6 and item.quality < 50:
            increase_quality(item)
    decrease_sell_in(item, 1)
    if item.sell_in < 0:
        item.quality = 0


def sulfuras(item):
    """
    Generate Sulfuras logic
    :param item:
    :return:
    """
    return item


def conjured(item):
    """
    Generate Conjured logic
    :param item:
    :return:
    """
    if item.quality > 0:
        decrease_quality(item, 2)
    decrease_sell_in(item)
    if item.sell_in < 0 < item.quality:
        decrease_quality(item, 2)


class GildedRose:
    """
    A class to represent the shop
    """

    def __init__(self, items):
        self.items = items

    def update_quality(self):
        """
        Update the quality of the given item
        :return:
        """
        for item in self.items:
            if item.name == constants.AGED_BRIE:
                aged_brie(item)
            elif item.name == constants.BACKSTAGE:
                backstage(item)
            elif item.name == constants.SULFURAS:
                sulfuras(item)
            elif item.name == constants.CONJURED:
                conjured(item)
            else:
                common_item(item)


class Item:
    """
    A class to represent the item itself
    """

    def __init__(self, name, sell_in, quality):
        self.name = name
        self.sell_in = sell_in
        self.quality = quality

    def __repr__(self):
        return "%s, %s, %s" % (self.name, self.sell_in, self.quality)
