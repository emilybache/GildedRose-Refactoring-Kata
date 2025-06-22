from abc import ABC, abstractmethod


class Writer(ABC):
    @abstractmethod
    def write_received_file(self, received_file: str) -> str:
        pass
