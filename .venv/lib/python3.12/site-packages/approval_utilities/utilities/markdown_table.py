from typing import TYPE_CHECKING, Any, Callable, Iterable

from typing_extensions import override

from approval_utilities.approvaltests.core.verifiable import Verifiable
from approval_utilities.approvaltests.core.verify_parameters import VerifyParameters

if TYPE_CHECKING:
    from approvaltests.core.options import Options


class MarkdownTable(Verifiable):
    def __init__(self) -> None:
        self.markdown: str = ""

    @override
    def get_verify_parameters(self, options: "Options") -> VerifyParameters:
        return VerifyParameters(options.for_file.with_extension(".md"))

    @staticmethod
    def with_headers(*column_names: str) -> "MarkdownTable":
        table = MarkdownTable()
        table.add_rows(*column_names)
        dividers = map(lambda _: "---", column_names)
        table.add_rows(*dividers)
        return table

    def add_rows(self, *column_names: str) -> "MarkdownTable":
        self.markdown += MarkdownTable.print_row(*column_names)
        return self

    @override
    def __str__(self) -> str:
        return self.markdown

    @staticmethod
    def print_row(*column_names: Any) -> str:
        row = "|"
        for column in column_names:
            row += f" {column} |"
        return row + "\n"

    def add_rows_for_inputs(
        self, inputs: Iterable[Any], *input_transformers: Callable[[Any], Any]
    ) -> "MarkdownTable":
        def transform_resolver_for_input(
            input_value: Any,
        ) -> Callable[[Callable[[Any], Any]], Any]:
            return lambda transform: transform(input_value)

        for row_input in inputs:
            row = [row_input]
            row += map(transform_resolver_for_input(row_input), input_transformers)
            self.add_rows(*row)
        return self
