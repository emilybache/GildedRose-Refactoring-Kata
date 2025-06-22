from typing import Any, Callable, List, Sequence

from approval_utilities.utilities.exceptions.multiple_exceptions import (
    MultipleExceptions,
)


class ExceptionCollector:
    def __init__(self) -> None:
        self._exceptions: List[Exception] = []

    def gather(self, code_to_execute: Callable[[], Any]) -> None:
        try:
            code_to_execute()
        except Exception as exception:
            self._exceptions.append(exception)

    def release(self) -> None:
        if len(self._exceptions) == 0:
            return
        if len(self._exceptions) == 1:
            raise self._exceptions[0]

        raise MultipleExceptions(self._exceptions)


def gather_all_exceptions(
    parameters: Sequence[Any], code_to_execute: Callable[[Any], Any]
) -> ExceptionCollector:
    collector = ExceptionCollector()
    for parameter in parameters:
        collector.gather(lambda p=parameter: code_to_execute(p))

    return collector


def gather_all_exceptions_and_throw(
    parameters: Sequence[Any], code_to_execute: Callable[[Any], Any]
) -> None:
    gather_all_exceptions(parameters, code_to_execute).release()
