import os
import textwrap
from dataclasses import dataclass
from typing import Iterable

from sybil import Document, Region, Example
from sybil.parsers.rest.lexers import DirectiveLexer

from testfixtures import diff


@dataclass
class FileBlock:
    path: str
    content: str
    action: str


class FileParser:
    """
    A `Sybil <http://sybil.readthedocs.io>`__ parser that
    parses certain ReST sections to read and write files in the
    configured :class:`~testfixtures.TempDirectory`.

    :param name: This is the name of the :class:`~testfixtures.TempDirectory` to use
                 in the Sybil test namespace.
    """

    def __init__(self, name: str):
        self.name = name
        self.lexer = DirectiveLexer('topic', arguments='.+')

    def __call__(self, document: Document) -> Iterable[Region]:
        for region in self.lexer(document):
            options = region.lexemes.get('options')
            if options is not None:
                class_ = options.get('class')
                if class_ in ('read-file', 'write-file'):
                    lines = region.lexemes['source'].splitlines(keepends=True)
                    index = 0
                    if lines[index].strip() == '::':
                        index += 1
                    source = textwrap.dedent(''.join(lines[index:])).lstrip()
                    if source[-1] != '\n':
                        source += '\n'
                    region.parsed = FileBlock(
                        path=region.lexemes['arguments'],
                        content=source,
                        action=class_.split('-')[0]
                    )
                    region.evaluator = self.evaluate
                    yield region

    def evaluate(self, example: Example):
        block: FileBlock = example.parsed
        temp_directory = example.namespace[self.name]
        if block.action == 'read':
            actual = temp_directory.as_path(block.path).read_text().replace(os.linesep, '\n')
            if actual != block.content:
                return diff(
                    block.content,
                    actual,
                    'File %r, line %i:' % (example.path, example.line),
                    'Reading from "%s":' % temp_directory.as_string(block.path)
                )
        if block.action == 'write':
            temp_directory.write(block.path, block.content)
