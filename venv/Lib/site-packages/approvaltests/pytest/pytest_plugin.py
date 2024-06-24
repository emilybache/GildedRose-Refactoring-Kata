from .pytest_config import set_pytest_config


def pytest_configure(config):
    set_pytest_config(config)
