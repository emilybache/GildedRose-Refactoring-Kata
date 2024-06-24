import datetime
import inspect
import traceback
from typing import Callable, Any, Iterable, ContextManager


from approval_utilities.utilities.exceptions.exception_utils import to_string
from approval_utilities.utilities.string_wrapper import StringWrapper
from approval_utilities.utilities.stack_frame_utilities import get_class_name_for_frame


class Toggles:
    def __init__(self, show: bool):
        self.queries = show
        self.messages = show
        self.variables = show
        self.hour_glass = show
        self.markers = show
        self.events = show


def _is_iterable(arg):
    return isinstance(arg, Iterable) and not isinstance(arg, str)


def print_type(value):
    return f"<{type(value).__name__}>"


class LoggingInstance:
    def __init__(self):
        self.log_stack_traces = True
        self.toggles = Toggles(True)
        self.previous_timestamp = None
        self.logger = lambda t: print(t, end="")
        self.tabbing = 0
        self.counter = 0
        self.log_with_timestamps = True
        self.timer: Callable[[], datetime.datetime] = datetime.datetime.now

    def log_to_string(self) -> StringWrapper:
        buffer = StringWrapper()
        self.logger = buffer.append
        self.log_with_timestamps = False
        self.log_stack_traces = False
        return buffer

    def indent(self) -> ContextManager:
        class Indent:
            def __init__(self, log):
                self.log = log

            def __enter__(self):
                self.log.tabbing += 1

            def __exit__(self, exc_type, exc_val, exc_tb):
                self.log.tabbing -= 1

        return Indent(self)

    def use_markers(
        self, parameter_text: [str, Callable[[], str]] = None, additional_stack: int = 0
    ) -> ContextManager:
        class Nothing:
            def __enter__(self):
                pass

            def __exit__(self, exc_type, exc_val, exc_tb):
                pass

        if not self.toggles.markers:
            return Nothing()

        class Markers:
            def __init__(self, log, method_name, filename, parameter_text):
                self.log = log
                self.method_name = method_name
                self.filename = filename
                self.parameter_text = parameter_text

            def __enter__(self):
                expected = f"-> in: {self.method_name}({self.get_parameters(False)}) in {self.filename}"
                self.log.log_line(expected)
                self.log.tabbing = self.log.tabbing + 1

            def __exit__(self, exc_type, exc_val, exc_tb):
                self.log.tabbing = self.log.tabbing - 1
                expected = f"<- out: {self.method_name}({self.get_parameters(True)})"
                self.log.log_line(expected)

            def get_parameters(self, is_exit: bool):
                if isinstance(self.parameter_text, Callable):
                    return parameter_text()
                elif self.parameter_text is None or is_exit:
                    return ""
                else:
                    return str(parameter_text)

        stack_position = 1 + additional_stack
        stack = inspect.stack(stack_position)[2]
        method_name = stack.function

        filename = get_class_name_for_frame(stack)
        return Markers(self, method_name, filename, parameter_text)

    def log_line(self, text: str, use_timestamps=True) -> None:
        if self.counter != 0:
            self.logger("\n")
            self.counter = 0
        timestamp = self.get_timestamp() if use_timestamps else ""
        output_message = f"{timestamp}{self.get_tabs()}{text}\n"
        self.logger(output_message)

    def get_timestamp(self) -> str:
        timestamp = ""
        if self.log_with_timestamps:
            time1: datetime.datetime = self.timer()
            time = time1.strftime("%Y-%m-%dT%H:%M:%SZ")
            diff_millseconds = 0
            if self.previous_timestamp != None:
                delta = time1 - self.previous_timestamp
                diff_millseconds = int((delta).total_seconds() * 1000)
            diff = diff_millseconds
            diff_display = f" ~{diff:06}ms"
            timestamp = f"[{time} {diff_display}] "
            self.previous_timestamp = time1
        return timestamp

    def hour_glass(self) -> None:
        if not self.toggles.hour_glass:
            return

        self.increment_hour_glass_counter()
        if self.counter == 1:
            self.logger(f"{self.get_tabs()}.")
        elif self.counter == 100:
            self.logger("10\n")
            self.counter = 0
        elif self.counter % 10 == 0:
            digit = int(self.counter / 10)
            self.logger(f"{digit}")
        else:
            self.logger(".")

    def get_tabs(self) -> str:
        return "  " * self.tabbing

    def increment_hour_glass_counter(self) -> None:
        self.counter = self.counter + 1

    def variable(self, name: str, value: Any, show_types: bool = False) -> None:
        if not self.toggles.variables:
            return

        def to_type(value: Any, spacing=" ") -> str:
            return f"{spacing}{print_type(value)}" if show_types else ""

        if _is_iterable(value):
            self.log_line(
                f"variable: {name}{to_type(value, spacing='')}.length = {len(value)}"
            )
            with self.indent():
                for i, v in enumerate(value):
                    self.logger(f"{self.get_tabs()}{name}[{i}] = {v}{to_type(v)}\n")

        else:
            self.log_line(f"variable: {name} = {value}{to_type(value)}")

    def event(self, event_name: str) -> None:
        if not self.toggles.events:
            return
        self.log_line(f"event: {event_name}")

    def query(self, query_text: str) -> None:
        if not self.toggles.queries:
            return
        self.log_line(f"Sql: {query_text}")

    def message(self, message):
        if not self.toggles.messages:
            return
        self.log_line(f"message: {message}")

    def warning(self, text: str = "", exception: BaseException = None) -> None:
        if isinstance(text, Exception):
            temp = ""
            if exception:
                temp = str(exception)
            exception = text
            text = temp

        warning_stars = "*" * 91
        self.log_line(warning_stars, use_timestamps=False)
        if self.log_with_timestamps:
            self.log_line("", use_timestamps=True)
        if text:
            self.log_line(f"Message:{text}", use_timestamps=False)
        if exception:
            if self.log_stack_traces:
                format_exception = traceback.format_exception(
                    None, exception, exception.__traceback__
                )
                stack_trace = "".join(format_exception)
            else:
                stack_trace = to_string(exception)
            self.log_line(stack_trace, use_timestamps=False)
        self.log_line(warning_stars, use_timestamps=False)

    def show_queries(self, show):
        self.toggles.queries = show

    def show_all(self, show: bool) -> None:
        self.toggles = Toggles(show)

    def show_messages(self, show):
        self.toggles.messages = show

    def show_variables(self, show):
        self.toggles.variables = show

    def show_hour_glass(self, show):
        self.toggles.hour_glass = show

    def show_markers(self, show):
        self.toggles.markers = show

    def show_events(self, show):
        self.toggles.events = show
