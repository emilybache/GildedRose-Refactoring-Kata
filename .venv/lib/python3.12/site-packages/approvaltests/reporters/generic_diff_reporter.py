from typing import List

from typing_extensions import override

from approval_utilities.utilities.os_utilities import run_command
from approval_utilities.utils import ensure_file_exists, filter_values, to_json
from approvaltests.command import Command
from approvaltests.core.reporter import Reporter
from approvaltests.reporters.generic_diff_reporter_config import (
    GenericDiffReporterConfig,
    create_config,
)

PROGRAM_FILES = "{ProgramFiles}"


class GenericDiffReporter(Reporter):
    """
    A reporter that launches
    an external diff tool given by config.
    """

    limit_count = 0
    diff_tool_limit = 5
    opened_diff_tool_count = 0

    @staticmethod
    def create(diff_tool_path: str) -> "GenericDiffReporter":
        return GenericDiffReporter(create_config(["custom", diff_tool_path]))

    def __init__(self, config: GenericDiffReporterConfig) -> None:
        self.name = config.name
        self.path = self.expand_program_files(config.path)
        self.extra_args = config.extra_args

    @override
    def __str__(self) -> str:
        config = filter_values(
            lambda v: bool(v),
            {"name": self.name, "path": self.path, "arguments": self.extra_args},
        )
        return to_json(config)

    @staticmethod
    def run_command(command_array: List[str]) -> None:
        run_command(command_array)

    def get_command(self, received: str, approved: str) -> List[str]:
        return [self.path] + self.extra_args + [received, approved]

    @override
    def report(self, received_path: str, approved_path: str) -> bool:
        if not self.is_working():
            return False

        GenericDiffReporter.opened_diff_tool_count += 1

        if (
            GenericDiffReporter.diff_tool_limit
            < GenericDiffReporter.opened_diff_tool_count
        ):
            GenericDiffReporter.limit_count += 1
            print(
                f"Skipping the diff because the limit[{GenericDiffReporter.diff_tool_limit}]"
                f" of opened diff reporters has been exceeded.\n"
                f"You can configure this with GenericDiffReporter.diff_tool_limit"
            )
            return True

        ensure_file_exists(approved_path)
        command_array = self.get_command(received_path, approved_path)
        self.run_command(command_array)
        return True

    def is_working(self) -> bool:
        found = Command(self.path).locate()
        if found:
            self.path = found
            return True
        return False

    @staticmethod
    def get_limit_count() -> int:
        return GenericDiffReporter.limit_count

    @staticmethod
    def expand_program_files(path: str) -> str:
        if PROGRAM_FILES not in path:
            return path

        for candidate in [
            r"C:/Program Files",
            r"C:/Program Files (x86)",
            r"C:/ProgramW6432",
        ]:
            possible = path.replace(PROGRAM_FILES, candidate)
            if Command.executable(possible):
                return possible
        return path.replace(PROGRAM_FILES, "C:/Program Files")

    @staticmethod
    def reset_opened_diff_tool_count() -> None:
        GenericDiffReporter.opened_diff_tool_count = 0
        GenericDiffReporter.limit_count = 0
