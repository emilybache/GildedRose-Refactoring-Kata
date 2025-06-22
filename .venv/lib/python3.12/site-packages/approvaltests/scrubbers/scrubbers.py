import re
from collections import abc, defaultdict
from typing import Callable, DefaultDict, Union

from approval_utilities.utilities.logger.logging_instance import print_type

Scrubber = Callable[[str], str]


# def print_type(value):
#     return f"<{type(value).__name__}>"
def create_regex_scrubber(
    regex: str, function_or_replace_string: Union[Callable[[int], str], str]
) -> Scrubber:
    if isinstance(function_or_replace_string, str):
        return lambda t: _replace_regex(t, regex, lambda _: function_or_replace_string)
    elif callable(function_or_replace_string):
        return lambda t: _replace_regex(t, regex, function_or_replace_string)
    else:
        raise TypeError(
            f"Parameter function_or_replace_string expects a string or callable, but got {print_type(function_or_replace_string)}"
        )


def _replace_regex(text: str, regex: str, replacement: Callable[[int], str]) -> str:
    matches = defaultdict(lambda: len(matches))  # type: DefaultDict[str, int]
    return re.sub(regex, lambda m: replacement(matches[m.group(0)]), text)


def scrub_all_dates(date: str) -> str:
    return create_regex_scrubber(
        r"\d{4}-\d{2}-\d{2}\ \d{2}:\d{2}:\d{2}", lambda t: f"<date{t}>"
    )(date)


def scrub_all_guids(data: str) -> str:
    return create_regex_scrubber(
        r"[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}",
        lambda t: f"<guid_{t}>",
    )(data)


def combine_scrubbers(*scrubbers: Scrubber) -> Scrubber:
    def combined(data: str) -> str:
        for scrubber in scrubbers:
            data = scrubber(data)
        return data

    return combined


def templates_regex_scrubber_with_lambda() -> Scrubber:
    """
    This method exists as a convenient way to get an example scrubber for you to use.
    To use this template, simply inline the method in your IDE.
    """
    return create_regex_scrubber(
        "your pattern here: [a-zA-Z]+/d{4}", lambda t: f"<your replacement_{t}>"
    )


def templates_regex_scrubber_with_replacement() -> Scrubber:
    """
    This method exists as a convient way to get an example scrubber for you to use.
    To use this template, simply inline the method in your IDE.
    """
    return create_regex_scrubber(
        "your pattern here: [a-zA-Z]+/d{4}", "<your replacement>"
    )


def create_line_scrubber(remove_lines_containing: str) -> Scrubber:
    return lambda t: scrub_lines_containing(remove_lines_containing, t)


def scrub_lines_containing(remove_lines_containing: str, text: str) -> str:
    lines = text.splitlines()
    for line in lines:
        if remove_lines_containing in line:
            lines.remove(line)
    return "\n".join(lines)
