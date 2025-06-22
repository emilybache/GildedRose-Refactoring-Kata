from typing import Optional

from approvaltests.namer.namer_base import NamerBase
from approvaltests.namer.stack_frame_namer import StackFrameNamer


def get_default_namer(extension: Optional[str] = None) -> NamerBase:
    return StackFrameNamer(extension)
