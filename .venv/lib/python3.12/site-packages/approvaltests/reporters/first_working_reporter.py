from typing import List

from typing_extensions import override

from approvaltests.core.reporter import Reporter


class FirstWorkingReporter(Reporter):
    """
    A composite reporter that goes through a list
    of reporters, running the first one that is
    working on the current machine.

    This is mostly an implementation detail of other
    classes in the library, but it may be useful in scenarios
    where a team wants to supply a list of custom reporter,
    and have the first working one of these be used.

    See also MultiReporter.
    """

    def __init__(self, *reporters: Reporter) -> None:
        self.reporters = reporters

    @override
    def report(self, received_path: str, approved_path: str) -> bool:
        for reporter in self.reporters:
            try:
                success = reporter.report(received_path, approved_path)
                if success:
                    return True
            except:  # pylint: disable=bare-except
                pass

        return False

    @override
    def __str__(self):
        reporters_string = ", ".join(str(s) for s in self.reporters)
        return f"FirstWorkingReporter({reporters_string})"

    __repr__ = __str__

    @override
    def __eq__(self, other) -> bool:
        return repr(self) == repr(other)
