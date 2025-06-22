import itertools
from typing import Any, Callable, Dict, Generator, Sequence


def first(sequence: Sequence[Any], predicate: Callable[[Any], bool]) -> Any:
    matching = filter(predicate, sequence)
    return next(matching, None)


def product_dict(**kwargs: Sequence[Any]) -> Generator[Dict[str, Any], None, None]:
    """
    Similar to `itertools.product`, but the resulting combinations retain the names.
    """
    keys = kwargs.keys()
    vals = kwargs.values()
    for instance in itertools.product(*vals):
        yield dict(zip(keys, instance))
