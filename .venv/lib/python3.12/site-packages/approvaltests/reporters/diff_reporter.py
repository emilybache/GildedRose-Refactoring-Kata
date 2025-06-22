from approvaltests.reporters.generic_diff_reporter_factory import (
    GenericDiffReporterFactory,
)

from .first_working_reporter import FirstWorkingReporter
from .python_native_reporter import PythonNativeReporter
from .report_with_diff_command_line import ReportWithDiffCommandLine
from .report_with_diff_tool_on_windows import ReportWithDiffToolOnWindows
from .report_with_vscode import ReportWithVSCode, ReportWithVSCodeMacOS


class DiffReporter(FirstWorkingReporter):
    """
    The DiffReporter class goes through a chain of possible diffing tools,
    to find the first option installed on your system.

    If none are found, it falls back to writing the diffs on
    the console.

    At present, the default Reporter is the DiffReporter.
    """

    def __init__(self, reporter_factory=None) -> None:
        factory = reporter_factory or GenericDiffReporterFactory()

        reporters = list(factory.get_all_reporters_from_config())
        reporters.extend(
            [
                ReportWithVSCode(),
                ReportWithVSCodeMacOS(),
                ReportWithDiffToolOnWindows(),
                ReportWithDiffCommandLine(),
                PythonNativeReporter(),
            ]
        )
        super(__class__, self).__init__(*reporters)
