import os
import subprocess
from typing import Sequence

from approvaltests import verify, Options


def verify_command_line_with_inputs(
    command, *, inputs: Sequence[any] = None, options: Options = None
):
    input_string = "\n".join(map(lambda a: f"{a}", inputs))
    verify_command_line(command, input_string=input_string, options=options)


def verify_command_line(
    command_line,
    *,  # enforce keyword arguments - https://www.python.org/dev/peps/pep-3102/
    input_string: str = None,
    options: Options = None,
    current_working_directory=".",
    additional_environment_variables=None,
):
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
