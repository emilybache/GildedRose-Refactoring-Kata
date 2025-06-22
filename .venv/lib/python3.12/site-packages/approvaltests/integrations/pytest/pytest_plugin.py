from .pytest_config import set_pytest_config


def pytest_configure(config: object) -> None:
    set_pytest_config(config)
