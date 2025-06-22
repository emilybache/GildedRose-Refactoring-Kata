from pathlib import Path

from approvaltests.internals.logs.log_commons import (
    APPROVAL_TESTS_TEMP_DIRECTORY,
    LogCommons,
)


class ApprovedFilesLog:
    @staticmethod
    def clear_log_file() -> None:
        ApprovedFilesLog.get_approved_files_log().write_text("")
        LogCommons.download_script_from_common_repo_if_needed(
            "remove_abandoned_files.py"
        )

    @staticmethod
    def get_approved_files_log() -> Path:
        return ApprovedFilesLog.get_temp_directory() / ".approved_files.log"

    @staticmethod
    def get_temp_directory() -> Path:
        APPROVAL_TESTS_TEMP_DIRECTORY.mkdir(parents=True, exist_ok=True)
        APPROVAL_TESTS_TEMP_DIRECTORY.joinpath(".gitignore").write_text("*")
        return APPROVAL_TESTS_TEMP_DIRECTORY

    @staticmethod
    def log(approved_file: str) -> None:
        with ApprovedFilesLog.get_approved_files_log().open(mode="a") as file:
            file.write(f"{approved_file}\n")


ApprovedFilesLog.clear_log_file()
