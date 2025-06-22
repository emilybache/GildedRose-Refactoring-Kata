from abc import ABC, abstractmethod

from approval_utilities.approvaltests.core.verify_parameters import VerifyParameters
from approvaltests.core.options import Options


class Verifiable(ABC):
    @abstractmethod
    def get_verify_parameters(self, options: "Options") -> VerifyParameters:
        pass
