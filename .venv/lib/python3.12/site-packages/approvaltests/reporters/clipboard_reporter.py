from typing import Optional

from typing_extensions import override

from approval_utilities.utilities.clipboard_utilities import copy_to_clipboard
from approval_utilities.utils import is_windows_os
from approvaltests.core.reporter import Reporter


def get_command_text(
    received_path: str, approved_path: str, is_windows: Optional[bool] = None
) -> str:
    if is_windows is None:
        is_windows = is_windows_os()
    if is_windows:
        command = f'move "{received_path}" "{approved_path}"'
    else:
        command = f"mv -f {received_path} {approved_path}"
    return command


class ClipboardReporter(Reporter):
    """
    A reporter that creates
    a command line suitable for approving
    the last failed test on systems that
    support terminal command 'mv', and puts
    the command on the clipboard, over-writes
    the previous clipboard contents.

    See also CommandLineReporter.
    """

    @override
    def report(self, received_path: str, approved_path: str) -> bool:
        text = get_command_text(received_path, approved_path)
        print(text)
        copy_to_clipboard(text)
        return True


class CommandLineReporter(Reporter):
    """
    A reporter that outputs a
    command line suitable for approving
    failing tests on systems that support
    terminal command 'mv'.

    The output is typically copied and pasted
    to a console window or script, to approve
    the new test results.

    See also ClipboardReporter.
    """

    @override
    def report(self, received_path: str, approved_path: str) -> bool:
        text = get_command_text(received_path, approved_path)
        print(f"\n\n{text}\n\n")
        return True
