from approvaltests.namer.inline_comparator import InlineComparator


def parse_docstring():
    lines = InlineComparator.get_test_method_doc_string().split("\n")[:-1]
    return [line.split("->")[0].strip() for line in lines]
