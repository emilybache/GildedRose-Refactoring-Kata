from typing import List, Optional


class GenericDiffReporterConfig:
    def __init__(self, name: str, path: str, extra_args: Optional[List[str]] = None):
        self.name = name
        self.path = path
        self.extra_args = extra_args or []

    def serialize(self) -> List[str]:
        result = [self.name, self.path]
        if self.extra_args:
            result.append(self.extra_args)
        return result


def create_config(config: List[str]) -> GenericDiffReporterConfig:
    return GenericDiffReporterConfig(*config)
