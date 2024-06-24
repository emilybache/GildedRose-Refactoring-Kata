import abc
import typing


class FormatWrapper(abc.ABC):
    @abc.abstractmethod
    def wrap(self, data: typing.Any) -> typing.Any:
        pass

    @abc.abstractmethod
    def is_match(self, data: typing.Any) -> bool:
        pass


class AlwaysMatch(FormatWrapper):
    def wrap(self, data: typing.Any) -> typing.Any:
        return data

    def is_match(self, data: typing.Any) -> bool:
        return True
