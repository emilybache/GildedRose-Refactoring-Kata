import shutil

from typing_extensions import override

from approvaltests.core import Reporter


class ReporterThatAutomaticallyApproves(Reporter):
    @override
    def report(self, received_path: str, approved_path: str) -> bool:
        shutil.move(received_path, approved_path)
        return True
