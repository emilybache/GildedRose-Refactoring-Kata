import argparse
from typing import Any

from typing_extensions import override

from approvaltests.core.format_wrapper import FormatWrapper


class FormatterWrapperOfArgparseNamespace(FormatWrapper):
    @override
    def wrap(self, data: Any) -> Any:
        return FormatterOfArgparseNamespace(data)

    @override
    def is_match(self, data: Any) -> bool:
        return isinstance(data, argparse.Namespace)


class FormatterOfArgparseNamespace:
    def __init__(self, result: Any):
        self.result = result

    @override
    def __str__(self) -> str:
        from approval_utilities.utils import to_json

        return to_json(vars(self.result))
