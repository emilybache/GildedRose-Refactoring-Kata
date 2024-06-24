from typing import Callable, List, Optional


def format_list(alist: List[str], formatter: Optional[Callable], header: str) -> str:
    if formatter is None:
        formatter = FormatLineItem().print_item
    text = (header + "\n\n") if header else ""
    for i in alist:
        text += formatter(i) + "\n"
    return text


class FormatLineItem(object):
    def __init__(self) -> None:
        self.index = 0

    def print_item(self, item: str) -> str:
        text = str(self.index) + ") " + str(item)
        self.index += 1
        return text
