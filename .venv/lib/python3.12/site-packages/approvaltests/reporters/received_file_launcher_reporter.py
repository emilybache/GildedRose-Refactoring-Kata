from subprocess import call
from typing import List

from typing_extensions import override

from approvaltests.core.reporter import Reporter


class ReceivedFileLauncherReporter(Reporter):
    """
    A reporter that attempts to
    open the received file using the
    system default file viewer.

    Depending on the file viewer being launched,
    the test suite execution may halt until the
    user has closed the new process.

    Note: only works on Windows for now.
    """

    @staticmethod
    def get_command(received_path: str) -> List[str]:
        return ["cmd", "/C", "start", received_path, "/B"]

    @override
    def report(self, received_path: str, approved_path: str) -> bool:
        command_array = self.get_command(received_path)
        call(command_array)
        return True
