from typing import Sequence, Any, Callable


def first(sequence: Sequence[Any], predicate: Callable[[Any], bool]) -> Any:
    matching = filter(predicate, sequence)
    return next(matching)
