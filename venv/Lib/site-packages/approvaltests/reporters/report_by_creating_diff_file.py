from pathlib import Path

from approval_utilities.utils import ensure_file_exists
from approvaltests.core.reporter import Reporter
from approvaltests.reporters.python_native_reporter import calculate_diff


class ReportByCreatingDiffFile(Reporter):
    def report(self, received_path: str, approved_path: str) -> bool:
        ensure_file_exists(approved_path)
        diff = calculate_diff(received_path, approved_path)
        with open(
            self.get_diff_file_name(received_path), mode="w", encoding="utf8"
        ) as file:
            file.write(diff)
        return True

    @staticmethod
    def get_diff_file_name(received_path):
        suffix = Path(received_path).suffix
        return received_path.replace(f".received{suffix}", ".diff")
