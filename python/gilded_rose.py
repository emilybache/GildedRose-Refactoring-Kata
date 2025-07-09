# -*- coding: utf-8 -*-
from item_wrappers import item_factory

class GildedRose(object):

    def __init__(self, items):
        self.items = items

    def update_quality(self):
        for item in self.items:
            wrapper = item_factory(item)
            wrapper.update()



