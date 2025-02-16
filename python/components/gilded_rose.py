from abc import abstractmethod, ABC
from pydantic import BaseModel


class Item(BaseModel):
    name : str
    sell_in : int
    quality : int

class GildedRose(ABC):
    def __init__(self, items: Item):
        self.name = items.name
        self.quality = items.quality
        self.sell_in = items.sell_in

    @abstractmethod
    def update_quality(self):
        pass

