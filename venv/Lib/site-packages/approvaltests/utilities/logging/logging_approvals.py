import logging
from datetime import datetime
from typing import Optional, ContextManager

from approvaltests import Options, verify
from testfixtures import LogCapture

from approvaltests.scrubbers.date_scrubber import DateScrubber


def verify_logging(
    *,  # enforce keyword arguments - https://www.python.org/dev/peps/pep-3102/
    options: Optional[Options] = None
) -> ContextManager:
    class VerifyLogging:
        def __init__(self):
            self.l = LogCapture()
            self.output = "anything"
            self.options = options if options else Options()

        def __enter__(self):
            self.l.__enter__()

            pass

        def __exit__(self, exc_type, exc_val, exc_tb):
            self.l.__exit__(exc_type, exc_val, exc_tb)
            self.options = self.options.add_scrubber(
                DateScrubber.get_scrubber_for("2023-07-16 17:39:03.293919")
            )
            verify(self.l, options=self.options)

    return VerifyLogging()
