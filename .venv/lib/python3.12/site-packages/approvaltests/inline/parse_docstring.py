from typing import List

from approvaltests.namer.inline_comparator import InlineComparator


def parse_docstring() -> List[str]:
    lines = InlineComparator.get_test_method_doc_string().split("\n")[:-1]
    return [line.split("->")[0].strip() for line in lines]
