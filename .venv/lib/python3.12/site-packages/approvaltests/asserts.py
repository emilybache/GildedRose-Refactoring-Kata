from typing import Any, Optional

from typing_extensions import override

from approval_utilities.utils import write_to_temporary_file
from approvaltests import FileApprover
from approvaltests.approvals import (
    get_default_namer,
    initialize_options,
    verify_with_namer,
)
from approvaltests.core import Reporter
from approvaltests.core.options import Options
from approvaltests.namer.stack_frame_namer import StackFrameNamer
from approvaltests.reporters.testing_reporter import ReporterForTesting


class FilePathNamer(StackFrameNamer):
    def __init__(self, file_path: str, extension: None = None) -> None:
        StackFrameNamer.__init__(self, extension)
        self.file_path = file_path

    @override
    def get_approved_filename(self, base: Optional[str] = None) -> str:
        return self.file_path


def assert_against_file(
    actual: str, file_path: str, reporter: Optional[ReporterForTesting] = None
) -> None:
    namer = FilePathNamer(file_path)
    FileApprover.add_allowed_duplicates(lambda n: n == namer.get_approved_filename())
    verify_with_namer(actual, namer, reporter)


def assert_equal_with_reporter(
    expected: str,
    actual: Any,
    reporter: Optional[Reporter] = None,
    *,  # enforce keyword arguments - https://www.python.org/dev/peps/pep-3102/
    options: Optional[Options] = None,
) -> None:
    options = initialize_options(options, reporter)
    actual = options.scrub(actual)
    if actual == expected:
        return

    name = get_default_namer().get_file_name()
    extention = options.for_file.file_extention
    expected_file = write_to_temporary_file(expected, name + ".expected.", extention)
    actual_file = write_to_temporary_file(actual, name + ".actual.", extention)
    options.reporter.report(actual_file, expected_file)
    raise AssertionError(
        f'expected != actual\n  actual: "{actual}"\nexpected: "{expected}"'
    )
