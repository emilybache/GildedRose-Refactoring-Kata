import fnmatch
import inspect
import os
from inspect import FrameInfo
from typing import Dict, List, Optional

from typing_extensions import override

from approval_utilities.utilities.stack_frame_utilities import get_class_name_for_frame
from approvaltests.approval_exception import FrameNotFound
from approvaltests.integrations.pytest.pytest_config import PytestConfig
from approvaltests.namer.namer_base import NamerBase


class StackFrameNamer(NamerBase):
    directory = ""
    method_name = ""
    class_name = ""

    def __init__(self, extension: Optional[str] = None) -> None:
        NamerBase.__init__(self, extension)
        self.set_for_stack(inspect.stack(1))
        self.config: Dict[str, str] = {}
        self.config_loaded = False

    def set_for_stack(self, caller: List[FrameInfo]) -> None:
        frame = self.get_test_frame_index(caller)
        stacktrace = caller[frame]
        self.method_name = stacktrace[3]
        self.class_name = get_class_name_for_frame(stacktrace)
        self.directory = os.path.dirname(stacktrace[1])

    @staticmethod
    def get_test_frame_index(caller: List[FrameInfo]) -> int:
        tmp_array = []
        for index, frame in enumerate(caller):
            if StackFrameNamer.is_test_method(frame):
                tmp_array.append(index)
        if tmp_array:
            return tmp_array[-1]
        message = """Could not find test method/function. Possible reasons could be:
1) approvaltests is not being used inside a test function
2) your test framework is not supported by ApprovalTests (unittest and pytest are currently supported)."""
        raise FrameNotFound(message)

    @staticmethod
    def is_pytest_test(frame: FrameInfo) -> bool:
        method_name = frame[3]
        patterns = PytestConfig.test_naming_patterns
        return StackFrameNamer._is_match_for_pytest(method_name, patterns)

    @staticmethod
    def _is_match_for_pytest(method_name: str, patterns: List[str]) -> bool:
        # Do not modify this method, so we can compare with original code
        # taken from pytest/python.py (class PyCollector)
        for pattern in patterns:
            if method_name.startswith(pattern):
                return True
                # Check that name looks like a glob-string before calling fnmatch
                # because this is called for every name in each collected module,
                # and fnmatch is somewhat expensive to call.
            elif (
                "*" in pattern or "?" in pattern or "[" in pattern
            ) and fnmatch.fnmatch(method_name, pattern):
                return True

        return False

    @staticmethod
    def is_unittest_test(frame: FrameInfo) -> bool:
        method_name = frame[3]
        local_attributes = frame[0].f_locals
        is_unittest_test = (
            "self" in local_attributes
            and hasattr(local_attributes["self"], "__dict__")
            and "_testMethodName" in vars(local_attributes["self"])
            and method_name != "__call__"
            and method_name != "_callTestMethod"
            and method_name != "run"
        )
        return is_unittest_test

    @staticmethod
    def is_test_method(frame: FrameInfo) -> bool:
        return StackFrameNamer.is_unittest_test(
            frame
        ) or StackFrameNamer.is_pytest_test(frame)

    def get_class_name(self) -> str:
        return self.class_name

    def get_method_name(self) -> str:
        return self.method_name

    @override
    def get_directory(self) -> str:
        return self.directory

    @override
    def config_directory(self) -> str:
        return self.directory

    @override
    def get_file_name(self) -> str:
        class_name = "" if (self.class_name is None) else (self.class_name + ".")
        return class_name + self.method_name

    def get_extension_with_dot(self) -> str:
        return self.extension_with_dot

    def get_extension_without_dot(self) -> str:
        return self.extension_with_dot[1:]

    @classmethod
    def get_test_frame(cls) -> FrameInfo:
        calling_stack = inspect.stack(1)
        frame = StackFrameNamer.get_test_frame_index(calling_stack)
        return calling_stack[frame]
