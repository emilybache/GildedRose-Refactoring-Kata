import re
from enum import Enum


class SplitCode:
    def __init__(self, before_method, after_method, tab):
        self.before_method = before_method
        self.after_method = after_method
        self.tab = tab

    def __str__(self):
        return f"before:\n{self.before_method}\nafter:\n{self.after_method}\ntab: '{self.tab}'\n"

    @staticmethod
    def on_method(code, method_name) -> "SplitCode":
        lines = code.split("\n")
        before = []
        after = []
        tab = "    "

        class State(Enum):
            BEFORE = 0
            FIRST_LINE_OF_METHOD_BODY = 1
            IN_DOCSTRING = 2
            AFTER_DOCTSTRING = 3

        state = State.BEFORE

        for line in lines:
            stripped_line = line.strip()
            if stripped_line.startswith(f"def {method_name}("):
                state = State.FIRST_LINE_OF_METHOD_BODY
                before.append(line)
            elif state == State.BEFORE:
                before.append(line)
            elif state == State.FIRST_LINE_OF_METHOD_BODY:
                tab = re.compile(r"^\s*").match(line).group()
                if stripped_line.startswith('"""'):
                    state = State.IN_DOCSTRING
                    doc_string_quotes = '"""'
                elif stripped_line.startswith("'''"):
                    state = State.IN_DOCSTRING
                    doc_string_quotes = "'''"
                else:
                    state = State.AFTER_DOCTSTRING
                    after.append(line)
            elif state == State.IN_DOCSTRING:
                if stripped_line.startswith(doc_string_quotes):
                    state = State.AFTER_DOCTSTRING
            elif state == State.AFTER_DOCTSTRING:
                after.append(line)
        return SplitCode("\n".join(before), "\n".join(after), tab)

    def indent(self, received_text):
        lines = received_text.split("\n")
        indented_lines = [f"{self.tab}{line}" for line in lines]
        return "\n".join(indented_lines)
