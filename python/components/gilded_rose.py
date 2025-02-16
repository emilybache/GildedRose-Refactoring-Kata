from abc import abstractmethod, ABC


class Item:
    def __init__(self, name, sell_in, quality):
        self.name = name
        self.sell_in = sell_in
        self.quality = quality

class GildedRose(ABC):
    def __init__(self, items: Item):
        self.name = items.name
        self.quality = items.quality
        self.sell_in = items.sell_in

    @abstractmethod
    def update_quality(self):
        pass

