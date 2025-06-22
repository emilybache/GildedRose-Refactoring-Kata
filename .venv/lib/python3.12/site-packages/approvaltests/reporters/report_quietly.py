from typing_extensions import override

from approvaltests.core import Reporter


class ReportQuietly(Reporter):
    @override
    def report(self, received_path: str, approved_path: str) -> bool:
        return True


QuietReport = ReportQuietly  # backwards compatibility
