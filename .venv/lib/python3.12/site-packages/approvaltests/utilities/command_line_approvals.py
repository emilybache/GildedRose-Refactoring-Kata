import os
import subprocess
from typing import Any, Dict, Optional, Sequence

from approvaltests import Options, verify


def verify_command_line_with_inputs(
    command: str,
    *,
    inputs: Optional[Sequence[Any]] = None,
    options: Optional[Options] = None,
) -> None:
    input_string = "\n".join(map(lambda a: f"{a}", inputs))
    verify_command_line(command, input_string=input_string, options=options)


def verify_command_line(
    command_line: str,
    *,  # enforce keyword arguments - https://www.python.org/dev/peps/pep-3102/
    input_string: Optional[str] = None,
    options: Optional[Options] = None,
    current_working_directory: str = ".",
    additional_environment_variables: Optional[Dict[str, str]] = None,
) -> None:
    my_env = None
    if additional_environment_variables:
        my_env = {**os.environ, **additional_environment_variables}
    verify(
        subprocess.check_output(
            command_line,
            shell=True,
            universal_newlines=True,
            input=input_string,
            cwd=current_working_directory,
            env=my_env,
        ),
        options=options,
    )
