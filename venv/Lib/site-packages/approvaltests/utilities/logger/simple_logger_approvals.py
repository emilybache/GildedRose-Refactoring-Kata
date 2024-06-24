from typing import ContextManager, Optional

from approval_utilities.utilities.logger.simple_logger import SimpleLogger
from approvaltests.approvals import verify
from approvaltests.core.options import Options


def verify_simple_logger(
    *,  # enforce keyword arguments - https://www.python.org/dev/peps/pep-3102/
    options: Optional[Options] = None
) -> ContextManager:
    class VerifySimpleLogger:
        def __init__(self):
            self.output = SimpleLogger.log_to_string()

        def __enter__(self):
            pass

        def __exit__(self, exc_type, exc_val, exc_tb):
            verify(self.output, options=options)

    return VerifySimpleLogger()
