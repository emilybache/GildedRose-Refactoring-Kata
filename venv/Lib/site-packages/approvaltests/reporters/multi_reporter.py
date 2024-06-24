from approvaltests.core.reporter import Reporter


class MultiReporter(Reporter):
    """
    A composite reporter that goes through a list
    of reporters, running all that are working on
    the current machine.

    See also FirstWorkingReporter.
    """

    def __init__(self, *reporters) -> None:
        self.reporters = reporters

    def report(self, received_path, approved_path):
        for reporter in self.reporters:
            reporter.report(received_path, approved_path)

    def __str__(self):
        return f"MultiReporter({', '.join([r.__class__.__name__ for r in self.reporters])})"
