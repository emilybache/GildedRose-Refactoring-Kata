from typing_extensions import override


class ApprovalException(Exception):
    def __init__(self, value: str) -> None:
        super().__init__(self)
        self.value = value

    @override
    def __str__(self) -> str:
        return self.value


class FrameNotFound(ApprovalException):
    pass
