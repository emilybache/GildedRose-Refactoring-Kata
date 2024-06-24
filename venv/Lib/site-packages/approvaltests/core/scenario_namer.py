from typing import Optional, Any

from approvaltests.core.namer import Namer
from approvaltests.namer.namer_base import NamerBase


class ScenarioNamer(Namer):
    """
    For use with parameterized tests.

    Use this namer when the same test case needs to verify more than one value, and produce more than one file.
    """

    def __init__(self, base_namer: NamerBase, *scenario_names: Any) -> None:
        self.base_namer = base_namer
        self.scenario_names = scenario_names

    def get_basename(self) -> str:
        basename = self.base_namer.get_basename()
        scenarios = ".".join(map(str, self.scenario_names))
        return f"{basename}.{scenarios}"

    def get_approved_filename(self, base: Optional[str] = None) -> str:
        base = base or self.get_basename()
        return self.base_namer.get_approved_filename(base)

    def get_received_filename(self, base: Optional[str] = None) -> str:
        base = base or self.get_basename()
        return self.base_namer.get_received_filename(base)

    def set_extension(self, extension: str) -> None:
        self.base_namer.set_extension(extension)
