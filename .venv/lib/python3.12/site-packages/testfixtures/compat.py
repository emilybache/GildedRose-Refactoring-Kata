# compatibility module for different python versions
import sys
from typing import Tuple

PY_VERSION: Tuple[int, int] = sys.version_info[:2]

PY_39_PLUS: bool = PY_VERSION >= (3, 9)
PY_310_PLUS: bool = PY_VERSION >= (3, 10)
PY_311_PLUS: bool = PY_VERSION >= (3, 11)
PY_312_PLUS: bool = PY_VERSION >= (3, 12)
