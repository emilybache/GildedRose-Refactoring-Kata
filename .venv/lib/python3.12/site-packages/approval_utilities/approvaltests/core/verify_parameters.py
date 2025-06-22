from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from approvaltests.core.options import Options


class VerifyParameters:
    def __init__(self, options: "Options"):
        self.options = options
