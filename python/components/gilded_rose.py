from abc import abstractmethod, ABC
from ..models import Items


class GildedRose(ABC):
    def __init__(self, item: str, sell_in: int, quality: int):
        self.item = item
        self.quality = quality
        self.sell_in = sell_in

    @abstractmethod
    def update_quality(self):
        pass

