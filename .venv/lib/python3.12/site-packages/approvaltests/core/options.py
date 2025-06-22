from typing import Callable, Dict, Optional

from approvaltests.core.comparator import Comparator
from approvaltests.core.namer import Namer
from approvaltests.core.reporter import Reporter
from approvaltests.file_approver import FileComparator
from approvaltests.inline.inline_options import InlineOptions
from approvaltests.scrubbers import combine_scrubbers


class FileOptions:
    def __init__(self, fields: Dict):
        self.fields = fields

    @property
    def file_extention(self) -> str:
        return self.fields.get("extension_with_dot", ".txt")

    def with_extension(
        self,
        extension_with_dot: str,
        *,  # enforce keyword arguments - https://www.python.org/dev/peps/pep-3102/,
        no_override: bool = False,
    ) -> "Options":
        if not extension_with_dot.startswith("."):
            extension_with_dot = "." + extension_with_dot
        if no_override and "extension_with_dot" in self.fields:
            extension_with_dot = self.fields["extension_with_dot"]
        return Options({**self.fields, **{"extension_with_dot": extension_with_dot}})


class Options:
    def __init__(self, fields: Optional[Dict] = None):
        self.fields = fields or {}

    @property
    def reporter(self) -> Reporter:
        from approvaltests.reporters.default_reporter_factory import (
            get_default_reporter,
        )

        return self.fields.get("reporter", get_default_reporter())

    @property
    def comparator(self) -> Comparator:
        return self.fields.get("comparator", FileComparator())

    def with_comparator(self, comparator: Comparator) -> "Options":
        return Options({**self.fields, **{"comparator": comparator}})

    def scrub(self, data: str) -> str:
        if self.has_scrubber():
            return self.fields["scrubber_func"](data)
        return data

    def with_scrubber(self, scrubber_func: Callable[[str], str]) -> "Options":
        return Options({**self.fields, **{"scrubber_func": scrubber_func}})

    def add_scrubber(self, scrubber: Callable[[str], str]) -> "Options":
        if self.has_scrubber():
            scrubber = combine_scrubbers(self.fields["scrubber_func"], scrubber)
        return self.with_scrubber(scrubber)

    def has_scrubber(self) -> bool:
        return "scrubber_func" in self.fields

    def with_reporter(self, reporter: "Reporter") -> "Options":
        return Options({**self.fields, **{"reporter": reporter}})

    def with_namer(self, namer: "Namer") -> "Options":
        return Options({**self.fields, **{"namer": namer}})

    @property
    def for_file(self) -> FileOptions:
        return FileOptions(self.fields)

    @property
    def namer(self) -> Namer:
        from approvaltests.namer.default_name import get_default_namer

        namer = self.fields.get("namer", get_default_namer())
        if hasattr(namer, "set_extension"):
            namer.set_extension(self.for_file.file_extention)
        return namer

    def inline(self, inline_options: Optional[InlineOptions] = None) -> "Options":
        from approvaltests.namer.inline_comparator import InlineComparator

        return InlineComparator().register(self, inline_options)

    def add_reporter(self, additional_reporter: Reporter) -> "Options":
        from approvaltests.reporters import MultiReporter

        return self.with_reporter(MultiReporter(self.reporter, additional_reporter))
