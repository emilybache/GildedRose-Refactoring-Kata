from typing import Any, Callable, Generic, List, Tuple

from approvaltests import Options, verify_all
from approvaltests.inline.parse2 import Parse2
from approvaltests.inline.parse3 import Parse3
from approvaltests.inline.types import T1, T2, T
from approvaltests.namer.inline_comparator import InlineComparator
from approvaltests.reporters import ReporterThatAutomaticallyApproves


class Parse(Generic[T]):
    def __init__(
        self, text: str, transformer: Callable[[str], T], options: Options
    ) -> None:
        self.text = text
        self._transformer = transformer
        self.options = options

    @staticmethod
    def doc_string(*, auto_approve: bool = False) -> "Parse[str]":
        options = Options()
        if auto_approve:
            options = options.with_reporter(ReporterThatAutomaticallyApproves())

        return Parse(
            InlineComparator.get_test_method_doc_string(), lambda s: s, options=options
        )

    def get_inputs(self) -> List[T]:
        return Parse.parse_inputs(self.text, self._transformer)

    @staticmethod
    def parse_inputs(text: str, transformer: Callable[[str], T]) -> List[T]:
        lines = text.split("\n")
        lines = list(filter(lambda line: line.strip(), lines))
        inputs = [line.split("->")[0].strip() for line in lines]
        return [transformer(i) for i in inputs]

    def verify_all(self, transform: Callable[[T], Any]) -> None:
        verify_all(
            "",
            self.get_inputs(),
            lambda s: f"{s} -> {transform(s)}",
            options=self.options.inline(),
        )

    def transform(self, transform: Callable[[T], T2]) -> "Parse[T2]":
        return Parse(self.text, lambda s: transform(self._transformer(s)), self.options)

    def transform2(
        self, transform1: Callable[[str], T1], transform2: Callable[[str], T2]
    ) -> "Parse2[T1, T2]":
        def transformer(s: str) -> Tuple[T1, T2]:
            parts = s.split(",")
            parts = list(map(lambda p: p.strip(), parts))
            return (transform1(parts[0]), transform2(parts[1]))

        return Parse2(self.text, transformer, self.options)

    # intention: find "cheating" parts in this code
    # TODO: make all hard-coded stuff be more generic
    # 'int1' is not used, let's start use it
    # add in callable
    # done: verify_all signature is incorrect
    def transform3(
        self,
        transform1: Callable[[str], T1],
        transform2: Callable[[str], T2],
        int1: Callable[[str], int],
    ) -> "Parse3[T1, int, int]":
        def transformer(s: str) -> Tuple[T1, T2, int]:
            parts = s.split(",")
            parts = list(map(lambda p: p.strip(), parts))
            return (transform1(parts[0]), transform2(parts[1]), int1(parts[2]))

        return Parse3(self.text, transformer, self.options)
