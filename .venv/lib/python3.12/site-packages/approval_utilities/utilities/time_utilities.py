import os
from types import TracebackType
from typing import Optional, Type

from typing_extensions import ContextManager


def use_utc_timezone() -> ContextManager[None]:
    class TimeZoneSwap:
        def __init__(self) -> None:
            self.timezone: Optional[str] = ""

        def __enter__(self) -> None:
            self.timezone = os.environ.get("TZ")
            os.environ["TZ"] = "UTC"

        def __exit__(
            self,
            exc_type: Optional[Type[BaseException]],
            exc_val: Optional[BaseException],
            exc_tb: Optional[TracebackType],
        ) -> bool:
            if self.timezone is None:
                os.environ.pop("TZ")
            else:
                os.environ["TZ"] = self.timezone

    return TimeZoneSwap()
