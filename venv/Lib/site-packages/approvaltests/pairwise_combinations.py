from typing import Any, Sequence


def get_best_covering_pairs(
    input_arguments: Sequence[Sequence[Any]],
) -> Sequence[Sequence[Any]]:
    from allpairspy import AllPairs

    return list(AllPairs(input_arguments))
