import os
from typing import Optional


class Command:
    def __init__(self, cmd: str) -> None:
        self.command = cmd

    @staticmethod
    def executable(cmd: str) -> bool:
        return os.path.isfile(cmd) and os.access(cmd, os.X_OK)

    def locate(self) -> Optional[str]:
        path, _ = os.path.split(self.command)
        if path and self.executable(self.command):
            return self.command

        for path in os.environ["PATH"].split(os.pathsep):
            path = path.strip('"')
            exe = os.path.join(path, self.command)
            if self.executable(exe):
                return exe

        return None
