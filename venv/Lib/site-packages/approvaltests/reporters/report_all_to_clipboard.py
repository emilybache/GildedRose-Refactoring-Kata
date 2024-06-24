from approvaltests.core.reporter import Reporter
from approvaltests.reporters.clipboard_reporter import get_command_text
from approval_utilities.utilities.clipboard_utilities import copy_to_clipboard


class ReporterByCopyMoveCommandForEverythingToClipboard(Reporter):
    text = ""

    def report(self, received_path, approved_path):
        ReporterByCopyMoveCommandForEverythingToClipboard.text = (
            ReporterByCopyMoveCommandForEverythingToClipboard.text
            + get_command_text(received_path, approved_path)
            + "\n"
        )
        copy_to_clipboard(ReporterByCopyMoveCommandForEverythingToClipboard.text)
        return True
