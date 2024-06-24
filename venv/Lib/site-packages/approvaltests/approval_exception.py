class ApprovalException(Exception):
    def __init__(self, value: str) -> None:
        super().__init__(self)
        self.value = value

    def __str__(self):
        return self.value


class FrameNotFound(ApprovalException):
    pass
