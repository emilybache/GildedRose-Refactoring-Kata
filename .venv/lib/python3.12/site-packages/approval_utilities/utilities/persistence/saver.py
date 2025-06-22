from abc import abstractmethod
from typing import Generic, TypeVar

T = TypeVar("T")


class Saver(Generic[T]):
    @abstractmethod
    def save(self, t: T) -> T:
        raise Exception("Interface member not implemented")
