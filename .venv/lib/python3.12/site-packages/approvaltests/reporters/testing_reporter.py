from typing_extensions import override

from approvaltests.core.reporter import Reporter


class ReporterForTesting(Reporter):
    def __init__(self) -> None:
        self.called = False

    @override
    def report(self, received_path: str, approved_path: str) -> bool:
        self.called = True
        return True
