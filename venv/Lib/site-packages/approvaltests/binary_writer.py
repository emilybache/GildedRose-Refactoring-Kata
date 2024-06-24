import io
from typing import ByteString

from approval_utilities.utils import create_directory_if_needed
from approvaltests.core.writer import Writer


class BinaryWriter(Writer):
    contents = ""

    def __init__(
        self,
        contents: ByteString,
        extension: str,
    ) -> None:
        self.contents = contents
        self.extension_with_dot = extension

    def write_received_file(self, received_file: str) -> str:
        create_directory_if_needed(received_file)
        with io.open(received_file, mode="wb") as file:
            file.write(self.contents)

        return received_file
