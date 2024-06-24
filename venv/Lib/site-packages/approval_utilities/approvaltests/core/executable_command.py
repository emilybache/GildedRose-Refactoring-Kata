from abc import abstractmethod, ABC


class CommandGetter(ABC):
    @abstractmethod
    def get_command(self) -> str:
        raise NotImplementedError("Interface member not implemented")


class CommandExecutor(ABC):
    @abstractmethod
    def execute_command(self, command: str) -> str:
        raise NotImplementedError("Interface member not implemented")


class ExecutableCommand(CommandGetter, CommandExecutor):
    pass
