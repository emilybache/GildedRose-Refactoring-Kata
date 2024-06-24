from typing import Optional

from approvaltests.core.namer import Namer


class CliNamer(Namer):
    def __init__(self, test_id: str) -> None:
        self.test_id = test_id

    def get_received_filename(self, base: Optional[str] = None) -> str:
        return f"{self.test_id}.received.txt"

    def get_approved_filename(self, base: Optional[str] = None) -> str:
        return f"{self.test_id}.approved.txt"

    def get_basename(self) -> str:
        return self.test_id
