import threading
from abc import ABC, abstractmethod
from typing import Callable, Generic, TypeVar, cast

from typing_extensions import override

_T = TypeVar("_T")


class Wrapper(ABC, Generic[_T]):
    @abstractmethod
    def get(self) -> _T:
        pass


class SingleWrapper(Wrapper[_T]):
    def __init__(self, instance: _T):
        self.instance = instance

    @override
    def get(self) -> _T:
        return self.instance


class ThreadedWrapper(Wrapper[_T]):
    def __init__(self, generator: Callable[[], _T]):
        self.generator = generator
        self.local = threading.local()

    @override
    def get(self) -> _T:
        if not hasattr(self.local, "value"):
            self.local.value = self.generator()
        return cast(_T, self.local.value)
