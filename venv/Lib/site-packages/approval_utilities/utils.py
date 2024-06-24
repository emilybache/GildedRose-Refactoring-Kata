import inspect
import json
import os
from copy import deepcopy


def get_adjacent_file(name: str) -> str:
    calling_file = inspect.stack(1)[1][1]
    directory = os.path.dirname(os.path.abspath(calling_file))
    return os.path.join(directory, name)


def write_to_temporary_file(text: str, name: str, file_extention_with_dot: str = None):
    import tempfile

    file_extention_with_dot = file_extention_with_dot or ".txt"
    with tempfile.NamedTemporaryFile(
        mode="w+b", suffix=file_extention_with_dot, prefix=name, delete=False
    ) as temp:
        temp.write(text.encode("utf-8-sig"))
        return temp.name


def to_json(object_to_verify) -> str:
    return json.dumps(
        object_to_verify,
        sort_keys=True,
        indent=4,
        separators=(",", ": "),
        default=lambda o: o.__dict__,
        ensure_ascii=True,
    )


def deserialize_json_fields(a_dict: dict) -> dict:
    a_dict = deepcopy(a_dict)
    for key, val in a_dict.items():
        if isinstance(val, str) and val.startswith("{"):
            try:
                deserialized_val = json.loads(val)
            except BaseException:
                # leave field unchanged on exception
                pass
            else:
                a_dict[key] = deserialized_val
        elif isinstance(val, dict):
            a_dict[key] = deserialize_json_fields(val)
    return a_dict


def is_windows_os() -> bool:
    return os.name == "nt"


def create_empty_file(file_path: str) -> None:
    import empty_files.empty_files

    empty_files.empty_files.create_empty_file(file_path)


def ensure_file_exists(approved_path: str) -> None:
    if not os.path.isfile(approved_path):
        create_empty_file(approved_path)


def create_directory_if_needed(received_file: str) -> None:
    directory = os.path.dirname(received_file)
    if directory and not os.path.exists(directory):
        os.makedirs(directory)


def print_grid(width, height, cell_print_func):
    result = ""
    for y in range(height):
        for x in range(width):
            result += cell_print_func(x, y)
        result += "\n"
    return result
