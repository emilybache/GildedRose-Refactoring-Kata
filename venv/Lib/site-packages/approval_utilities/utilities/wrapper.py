import threading
from abc import ABC, abstractmethod


class Wrapper(ABC):
    @abstractmethod
    def get(self):
        pass


class SingleWrapper(Wrapper):
    def __init__(self, instance):
        self.instance = instance

    def get(self):
        return self.instance


class ThreadedWrapper(Wrapper):
    def __init__(self, generator):
        self.generator = generator
        self.local = threading.local()

    def get(self):
        if not hasattr(self.local, "value"):
            self.local.value = self.generator()
        return self.local.value
