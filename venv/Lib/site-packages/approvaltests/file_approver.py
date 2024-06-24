import filecmp
import os
import pathlib
from typing import Optional, Callable

from approval_utilities.utilities.multiline_string_utils import remove_indentation_from
from approvaltests.core.comparator import Comparator
from approvaltests.core.namer import Namer
from approvaltests.core.reporter import Reporter
from approvaltests.core.writer import Writer


def exists(path: str) -> bool:
    return os.path.isfile(path)


class ReporterNotWorkingException(Exception):
    def __init__(self, reporter: Reporter):
        super().__init__(f"Reporter {reporter} failed to work!")


class FileComparator(Comparator):
    def compare(self, received_path: str, approved_path: str) -> bool:
        if not exists(approved_path) or not exists(received_path):
            return False
        if filecmp.cmp(approved_path, received_path, shallow=False):
            return True
        try:
            approved_raw = pathlib.Path(approved_path).read_text()
            approved_text = approved_raw.replace("\r\n", "\n")
            received_raw = pathlib.Path(received_path).read_text()
            received_text = received_raw.replace("\r\n", "\n")
            return approved_text == received_text
        except BaseException:
            return False


class FileApprover:
    previous_approved = []
    allowed_duplicates = []

    @staticmethod
    def verify(
        namer: Namer, writer: Writer, reporter: Reporter, comparator: Comparator
    ) -> Optional[str]:
        approved = namer.get_approved_filename()
        received = namer.get_received_filename()

        if FileApprover.is_this_a_multiple_verify(approved):
            return FileApprover.get_duplicate_verify_error_message(approved)
        FileApprover.previous_approved.append(approved)

        # The writer has the ability to change the name of the received file
        received = writer.write_received_file(received)
        verified = FileApprover.verify_files(approved, received, reporter, comparator)

        if not verified:
            return (
                f"Approval Mismatch, received != approved\n"
                f"\tApproved: {approved}\n"
                f"\tReceived: {received} "
            )
        return None

    @staticmethod
    def get_duplicate_verify_error_message(approved):
        return remove_indentation_from(
            f"""
            We noticed that you called verify more than once in the same test. 
            This is the second call to verify:
                approved_file: {approved}
    
            By default, ApprovalTests only allows one verify() call per test.
            To find out more, visit: 
            https://github.com/approvals/ApprovalTests.Python/blob/main/docs/how_to/multiple_approvals_per_test.md
            
            # Possible Fixes
            1. Separate your test into two tests
            2. In your verify call, add `options=NamerFactory.with_parameters("your_paramater")`
            3. In your test, call `approvals.settings().allow_multiple_verify_calls_for_this_method()`
            """
        )

    @staticmethod
    def is_this_a_multiple_verify(approved):
        return (
            approved in FileApprover.previous_approved
            and not FileApprover.is_duplicate_allowed(approved)
        )

    @staticmethod
    def is_duplicate_allowed(approved):
        for allowed in FileApprover.allowed_duplicates:
            if allowed(approved):
                return True
        return False

    @staticmethod
    def verify_files(
        approved_file: str,
        received_file: str,
        reporter: Reporter,
        comparator: Comparator,
    ) -> bool:
        if comparator.compare(received_file, approved_file):
            os.remove(received_file)
            return True

        worked = reporter.report(received_file, approved_file)
        if not worked:
            raise ReporterNotWorkingException(reporter)
        return False

    @staticmethod
    def add_allowed_duplicates(is_duplicate_allowed: Callable[[str], bool]):
        FileApprover.allowed_duplicates.append(is_duplicate_allowed)
