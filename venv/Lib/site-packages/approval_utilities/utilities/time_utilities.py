import os

from typing_extensions import ContextManager


def use_utc_timezone() -> ContextManager:
    class TimeZoneSwap:
        def __init__(self):
            self.timezone = ""

        def __enter__(self):
            self.timezone = os.environ.get("TZ")
            os.environ["TZ"] = "UCT"

        def __exit__(self, exc_type, exc_val, exc_tb):
            if self.timezone is None:
                os.environ.pop("TZ")
            else:
                os.environ["TZ"] = self.timezone

    return TimeZoneSwap()
