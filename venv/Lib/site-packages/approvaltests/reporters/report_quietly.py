from approvaltests.core import Reporter


class ReportQuietly(Reporter):
    def report(self, received_path: str, approved_path: str) -> bool:
        return True


QuietReport = ReportQuietly  # backwards compatibility
