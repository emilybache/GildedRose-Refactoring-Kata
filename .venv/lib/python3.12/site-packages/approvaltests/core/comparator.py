from abc import ABC, abstractmethod

from typing_extensions import override


class Comparator(ABC):
    """
    Super class of all Comparators in ApprovalTests.Python

    The only necessary function to implement is
    'compare', which takes the absolute
    paths of the received- and approved files, and
    returns a truthy value on success.
    """

    @override
    def __eq__(self, other: object) -> bool:
        return repr(self) == repr(other)

    @abstractmethod
    def compare(self, received_path: str, approved_path: str) -> bool:
        """
        Checks if two files contain the same information
        """
        raise Exception("Interface member not implemented")
