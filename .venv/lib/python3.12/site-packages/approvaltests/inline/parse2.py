from typing import Any, Callable, Generic, Tuple

from approvaltests import verify_all
from approvaltests.core.options import Options
from approvaltests.inline.types import NT1, NT2, T1, T2


class Parse2(Generic[T1, T2]):
    def __init__(
        self, text: str, transformer: Callable[[str], Tuple[T1, T2]], options: Options
    ) -> None:
        self.text = text
        self._transformer = transformer
        self.options = options

    def verify_all(self, transform: Callable[[T1, T2], Any]) -> None:
        from approvaltests.inline.parse import Parse

        verify_all(
            "",
            Parse.parse_inputs(self.text, self._transformer),
            lambda s: f"{s[0]}, {s[1]} -> {transform(s[0], s[1])}",
            options=self.options.inline(),
        )

    def transform2(
        self, transform1: Callable[[T1], NT1], transform2: Callable[[T2], NT2]
    ) -> "Parse2[NT1, NT2]":
        def transformer(s: str) -> Tuple[NT1, NT2]:
            t1, t2 = self._transformer(s)
            return (transform1(t1), transform2(t2))

        return Parse2(self.text, transformer, self.options)
