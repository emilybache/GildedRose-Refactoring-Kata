import argparse

from approvaltests.core.format_wrapper import FormatWrapper


class FormatterWrapperOfArgparseNamespace(FormatWrapper):
    def wrap(self, data):
        return FormatterOfArgparseNamespace(data)

    def is_match(self, data) -> bool:
        return isinstance(data, argparse.Namespace)


class FormatterOfArgparseNamespace:
    def __init__(self, result):
        self.result = result

    def __str__(self):
        from approval_utilities.utils import to_json

        return to_json(vars(self.result))
