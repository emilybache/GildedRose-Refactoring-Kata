from approvaltests.reporters.first_working_reporter import FirstWorkingReporter
from approvaltests.reporters.generic_diff_reporter import (
    GenericDiffReporter,
)
from approvaltests.reporters.generic_diff_reporter_config import (
    GenericDiffReporterConfig,
)


class ReportWithBeyondCompareLinux(GenericDiffReporter):
    def __init__(self):
        super().__init__(
            config=GenericDiffReporterConfig(
                name=self.__class__.__name__, path="/usr/bin/bcompare"
            )
        )


class ReportWithBeyondCompareMac(GenericDiffReporter):
    def __init__(self):
        super().__init__(
            config=GenericDiffReporterConfig(
                name=self.__class__.__name__, path="/usr/bin/bcompare"
            )
        )


class ReportWithBeyondCompareWindows(GenericDiffReporter):
    def __init__(self):
        super().__init__(
            config=GenericDiffReporterConfig(
                name=self.__class__.__name__,
                path="{ProgramFiles}/Beyond Compare 4/BCompare.exe",
            )
        )


class ReportWithWinMerge(GenericDiffReporter):
    def __init__(self):
        super().__init__(
            config=GenericDiffReporterConfig(
                name=self.__class__.__name__,
                path="{ProgramFiles}/WinMerge/WinMergeU.exe",
            )
        )


class ReportWithPycharm(GenericDiffReporter):
    def __init__(self):
        super().__init__(
            config=GenericDiffReporterConfig(
                name=self.__class__.__name__,
                path="{ProgramFiles}/JetBrains/PyCharm 2021.2.2/bin/pycharm64.exe",
                extra_args=["diff"],
            )
        )


class ReportWithBeyondCompare(FirstWorkingReporter):
    def __init__(self):
        super().__init__(
            ReportWithBeyondCompareMac(),
            ReportWithBeyondCompareWindows(),
            ReportWithBeyondCompareLinux(),
        )


def report_with_beyond_compare() -> ReportWithBeyondCompare:
    return ReportWithBeyondCompare()
