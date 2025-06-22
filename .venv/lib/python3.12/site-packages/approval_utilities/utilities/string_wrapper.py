from typing_extensions import override


class StringWrapper:
    def __init__(self) -> None:
        self.string = ""

    def append(self, text: str) -> None:
        self.string += text

    @override
    def __str__(self) -> str:
        return self.string

    @override
    def __repr__(self) -> str:
        return self.string
