import json
import os
from abc import abstractmethod
from typing import Dict, Optional

from typing_extensions import override

from approvaltests.core.namer import Namer


class NamerBase(Namer):
    def __init__(self, extension: Optional[str] = None) -> None:
        self.extension_with_dot = extension or ".txt"
        self.config_loaded = False
        self.config: Optional[Dict[str, str]] = None

    @abstractmethod
    def get_file_name(self) -> str:
        raise Exception("This class is abstract, override this method in a subclass")

    @abstractmethod
    def get_directory(self) -> str:
        raise Exception("This class is abstract, override this method in a subclass")

    def config_directory(self) -> str:
        # pylint:disable=no-self-use
        return None

    def get_config(self) -> Dict[str, str]:
        """lazy load config when we need it, then store it in the instance variable self.config"""
        if not self.config_loaded:
            config_file = os.path.join(
                self.config_directory(), "approvaltests_config.json"
            )
            if os.path.exists(config_file):
                with open(config_file, "r", encoding="utf8") as file:
                    self.config = json.load(file)
            else:
                self.config = {}
            self.config_loaded = True
        return self.config

    def get_basename(self) -> str:
        file_name = self.get_file_name()
        subdirectory = self.get_config().get("subdirectory", "")
        return str(os.path.join(self.get_directory(), subdirectory, file_name))

    @override
    def get_received_filename(self, base: Optional[str] = None) -> str:
        base = base or self.get_basename()
        return base + Namer.RECEIVED + self.extension_with_dot

    @override
    def get_approved_filename(self, base: Optional[str] = None) -> str:
        base = base or self.get_basename()
        return base + Namer.APPROVED + self.extension_with_dot

    def set_extension(self, extension: str) -> None:
        self.extension_with_dot = extension
