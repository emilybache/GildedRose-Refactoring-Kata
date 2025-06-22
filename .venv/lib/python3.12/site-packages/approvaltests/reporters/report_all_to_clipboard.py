from typing_extensions import override

from approval_utilities.utilities.clipboard_utilities import copy_to_clipboard
from approvaltests.core.reporter import Reporter
from approvaltests.reporters.clipboard_reporter import get_command_text


class ReporterByCopyMoveCommandForEverythingToClipboard(Reporter):
    text: str = ""

    @override
    def report(self, received_path: str, approved_path: str) -> bool:
        ReporterByCopyMoveCommandForEverythingToClipboard.text = (
            ReporterByCopyMoveCommandForEverythingToClipboard.text
            + get_command_text(received_path, approved_path)
            + "\n"
        )
        copy_to_clipboard(ReporterByCopyMoveCommandForEverythingToClipboard.text)
        return True
