import shutil
from pathlib import Path

from typing_extensions import override

from approvaltests.core import Options, Writer


class ExistingFileWriter(Writer):
    def __init__(self, file_name: str, options: Options) -> None:
        self.file_name = file_name
        self.options = options

    @override
    def write_received_file(self, received_file: str) -> str:
        if not self.options.has_scrubber():
            shutil.copyfile(self.file_name, received_file)
        else:
            text = Path(self.file_name).read_text()
            scrubbed_text = self.options.scrub(text)
            Path(received_file).write_text(scrubbed_text)
        return received_file
