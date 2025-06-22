import subprocess
from typing import List


def run_command(command_array: List[str]) -> None:
    with subprocess.Popen(command_array):
        pass
