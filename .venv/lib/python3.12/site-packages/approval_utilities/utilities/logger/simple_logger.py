import threading
from typing import Any, Callable, ContextManager, Iterator, Optional, Union

from approval_utilities.utilities.logger.logging_instance import LoggingInstance
from approval_utilities.utilities.string_wrapper import StringWrapper
from approval_utilities.utilities.wrapper import SingleWrapper, ThreadedWrapper, Wrapper


class SimpleLogger:
    _wrapper: Wrapper[LoggingInstance] = SingleWrapper(LoggingInstance())

    @staticmethod
    def register_logger(log_method: Callable[[str], None]) -> None:
        SimpleLogger._wrapper.get().logger = log_method

    @staticmethod
    def log_to_string(log_separate_threads: bool = True) -> StringWrapper:
        with threading.Lock():
            if log_separate_threads and not isinstance(
                SimpleLogger._wrapper, ThreadedWrapper
            ):
                SimpleLogger._wrapper = ThreadedWrapper(lambda: LoggingInstance())
        return SimpleLogger._wrapper.get().log_to_string()

    @staticmethod
    def use_markers(
        parameter_text: Optional[Union[str, Callable[[], str]]] = None,
    ) -> ContextManager[None]:
        return SimpleLogger._wrapper.get().use_markers(
            parameter_text, additional_stack=1
        )

    @staticmethod
    def variable(name: str, value: Any, show_types: bool = False) -> None:
        SimpleLogger._wrapper.get().variable(name, value, show_types=show_types)

    @staticmethod
    def hour_glass() -> None:
        SimpleLogger._wrapper.get().hour_glass()

    @staticmethod
    def event(event_name: str) -> None:
        SimpleLogger._wrapper.get().event(event_name)

    @staticmethod
    def show_timestamps(display: bool) -> None:
        SimpleLogger._wrapper.get().log_with_timestamps = display

    @staticmethod
    def query(query_text: str) -> None:
        SimpleLogger._wrapper.get().query(query_text)

    @staticmethod
    def message(message: str) -> None:
        SimpleLogger._wrapper.get().message(message)

    @staticmethod
    def warning(
        text: Union[str, BaseException] = "", exception: Optional[BaseException] = None
    ) -> None:
        SimpleLogger._wrapper.get().warning(text, exception)

    @staticmethod
    def show_queries(show: bool) -> None:
        SimpleLogger._wrapper.get().show_queries(show)

    @staticmethod
    def show_all(show: bool) -> None:
        SimpleLogger._wrapper.get().show_all(show)

    @staticmethod
    def show_messages(show: bool) -> None:
        SimpleLogger._wrapper.get().show_messages(show)

    @staticmethod
    def show_variables(show: bool) -> None:
        SimpleLogger._wrapper.get().show_variables(show)

    @staticmethod
    def show_hour_glass(show: bool) -> None:
        SimpleLogger._wrapper.get().show_hour_glass(show)

    @staticmethod
    def show_markers(show: bool) -> None:
        SimpleLogger._wrapper.get().show_markers(show)

    @staticmethod
    def show_events(show: bool) -> None:
        SimpleLogger._wrapper.get().show_events(show)
