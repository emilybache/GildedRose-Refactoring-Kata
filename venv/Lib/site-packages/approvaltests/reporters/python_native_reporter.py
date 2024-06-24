#!/usr/bin/env python

import os
import sys
from difflib import unified_diff

from approvaltests.reporters.clipboard_reporter import get_command_text
from approvaltests.core.reporter import Reporter
from approval_utilities.utils import ensure_file_exists


class PythonNativeReporter(Reporter):
    """
    A reporter that outputs diff straight
    to standard output.

    This is useful when running in a non-GUI environment,
    such as in Continuous Integration systems.
    """

    def report(self, received_path: str, approved_path: str) -> bool:
        ensure_file_exists(approved_path)
        print(calculate_diff_with_approve_instruction(received_path, approved_path))
        return True

    def __str__(self):
        return self.__class__.__name__

    __repr__ = __str__


def calculate_diff_with_approve_instruction(file1: str, file2: str):
    diff_string = calculate_diff(file1, file2)
    if diff_string.strip():
        approve = get_command_text(file1, file2)
        approve_cmd = "\n\nto approve this result:\n\n" + approve + "\n"
    else:
        approve_cmd = ""
    return diff_string + approve_cmd


def calculate_diff(filename1: str, filename2: str):
    with open(filename1, encoding="utf8") as file1:
        with open(filename2, encoding="utf8") as file2:
            diff = unified_diff(
                file2.readlines(),
                file1.readlines(),
                os.path.basename(filename2),
                os.path.basename(filename1),
            )
            diff_string = "".join(diff)
            return diff_string


if __name__ == "__main__":
    print(calculate_diff_with_approve_instruction(sys.argv[1], sys.argv[2]))
