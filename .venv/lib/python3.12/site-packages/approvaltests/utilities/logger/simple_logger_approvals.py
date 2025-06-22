from types import TracebackType
from typing import ContextManager, Optional, Type

from approval_utilities.utilities.logger.simple_logger import SimpleLogger
from approvaltests.approvals import verify
from approvaltests.core.options import Options


def verify_simple_logger(
    *,  # enforce keyword arguments - https://www.python.org/dev/peps/pep-3102/
    options: Optional[Options] = None
) -> ContextManager[None]:
    class VerifySimpleLogger:
        def __init__(self) -> None:
            self.output = SimpleLogger.log_to_string()

        def __enter__(self) -> None:
            pass

        def __exit__(
            self,
            exc_type: Optional[Type[BaseException]],
            exc_val: Optional[BaseException],
            exc_tb: Optional[TracebackType],
        ) -> bool:
            verify(self.output, options=options)

    return VerifySimpleLogger()
