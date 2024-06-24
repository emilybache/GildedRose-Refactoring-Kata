from typing import Sequence

from approval_utilities.utilities.exceptions.exception_utils import to_string


class MultipleExceptions(Exception):
    def __init__(self, exceptions: Sequence[Exception]):
        msg = "\n  " + "\n  ".join(map(to_string, exceptions))
        super().__init__(msg)
