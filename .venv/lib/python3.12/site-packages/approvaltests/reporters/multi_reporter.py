from typing_extensions import override

from approvaltests.core.reporter import Reporter


class MultiReporter(Reporter):
    """
    A composite reporter that goes through a list
    of reporters, running all that are working on
    the current machine.

    See also FirstWorkingReporter.
    """

    def __init__(self, *reporters: Reporter) -> None:
        self.reporters = reporters

    @override
    def report(self, received_path: str, approved_path: str) -> bool:
        for reporter in self.reporters:
            reporter.report(received_path, approved_path)

    @override
    def __str__(self) -> str:
        return f"MultiReporter({', '.join([r.__class__.__name__ for r in self.reporters])})"
