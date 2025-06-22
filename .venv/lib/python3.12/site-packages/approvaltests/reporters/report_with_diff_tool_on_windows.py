from approvaltests.reporters.first_working_reporter import FirstWorkingReporter
from approvaltests.reporters.report_with_beyond_compare import ReportWithBeyondCompare


class ReportWithDiffToolOnWindows(FirstWorkingReporter):

    def __init__(self) -> None:
        super().__init__(ReportWithBeyondCompare())
