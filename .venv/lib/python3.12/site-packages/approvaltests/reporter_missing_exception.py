from typing_extensions import override


class ReporterMissingException(BaseException):
    def __init__(self, key: str) -> None:
        super().__init__(self)
        self.value = key

    @override
    def __str__(self) -> str:
        return f"Could not find {repr(self.value)} in the environment, perhaps you need to configure your reporter."
