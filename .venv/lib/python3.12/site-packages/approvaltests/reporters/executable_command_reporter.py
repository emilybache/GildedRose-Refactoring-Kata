import pathlib
from typing import Optional

from typing_extensions import override

from approval_utilities.approvaltests.core.executable_command import ExecutableCommand
from approval_utilities.utilities.multiline_string_utils import remove_indentation_from
from approvaltests.core.reporter import Reporter


class ExecutableCommandReporter(Reporter):
    def __init__(self, executor: ExecutableCommand, reporter: Reporter):
        self.executor = executor
        self.reporter = reporter

    @override
    def report(self, received_filename: str, approved_filename: str) -> bool:
        # recieved and approved commands are not the same
        # todo run the content of the file against the executable command
        # execute(read(received))

        self.reporter.report(
            self.execute_result(received_filename),
            self.execute_result(approved_filename),
        )
        self.reporter.report(received_filename, approved_filename)
        return True

    def execute_result(self, filename: str) -> str:
        path = pathlib.Path(filename)
        command_string = None
        if path.exists():
            command_string = path.read_text()
        result = ExecutableCommandReporter.execute_command_and_format_result(
            command_string, self.executor
        )
        approved_executed_result_file = (
            f"{path.name[:-len(path.suffix)]}.executed_results.txt"
        )
        pathlib.Path(approved_executed_result_file).write_text(result)
        return approved_executed_result_file

    @staticmethod
    def execute_command_and_format_result(
        my_command: Optional[str], executor: ExecutableCommand
    ) -> str:
        if not my_command:
            return ""

        result = executor.execute_command(my_command)
        return (
            remove_indentation_from(
                f"""
                Do NOT approve
                This File will be Deleted
                it is for feedback purposes only
        
        command: {my_command}
        
        result:
        """
            )
            + result
        )
