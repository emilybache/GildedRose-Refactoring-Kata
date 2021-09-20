# -*- coding: utf-8 -*-
from abc import ABC, abstractmethod


class Item:
    def __init__(self, name, sell_in, quality):
        self.name = name
        self.sell_in = sell_in
        self.quality = quality

    def __repr__(self):
        return "%s, %s, %s" % (self.name, self.sell_in, self.quality)

    @abstractmethod
    def evaluate(self):
        pass

    def increment(self):
        self.quality += 1

    def decrement(self):
        self.quality -= 1

    def sold(self):
        self.sell_in -= 1


class AgedBrie(Item):
    def evaluate(self):
        if self.quality < 50:
            self.increment()
        self.sold()
        if self.sell_in < 0 and self.quality < 50:
            self.increment()


class Backstage(Item):
    def evaluate(self):
        if self.quality < 50:
            self.increment()
        if self.sell_in < 11:
            if self.quality < 50:
                self.increment()
        if self.sell_in < 6:
            if self.quality < 50:
                self.increment()
        self.sold()
        if self.sell_in < 0 and self.quality < 50:
            self.increment()


class Sulfuras(Item):
    def evaluate(self):
        if self.quality < 50:
            self.increment()
        if self.sell_in < 0 and self.quality < 50:
            self.increment()


class General(Item):
    def evaluate(self):
        if self.quality > 0:
            self.decrement()
        self.sold()
        if self.sell_in < 0:
            if self.quality > 0:
                self.decrement()
