import os
from inspect import FrameInfo


def get_class_name_for_frame(stacktrace: FrameInfo) -> str:
    if "self" not in stacktrace[0].f_locals:
        name = os.path.splitext(os.path.basename(stacktrace[1]))[0]
    else:
        name = f"{stacktrace[0].f_locals['self'].__class__.__name__}"
    return name
