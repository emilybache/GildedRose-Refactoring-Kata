from abc import abstractmethod
from typing import Generic, TypeVar

T = TypeVar("T")


class Loader(Generic[T]):
    @abstractmethod
    def load(self) -> T:
        raise Exception("Interface member not implemented")
