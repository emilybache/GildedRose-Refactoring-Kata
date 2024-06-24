from abc import ABC, abstractmethod

from approval_utilities.approvaltests.core.verify_parameters import VerifyParameters


class Verifiable(ABC):
    @abstractmethod
    def get_verify_parameters(self, options: "Options") -> VerifyParameters:
        pass
