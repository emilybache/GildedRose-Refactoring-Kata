class PytestConfig:
    test_naming_patterns = ["test_*"]

    @staticmethod
    def set_config(config):
        PytestConfig.test_naming_patterns = config.getini("python_functions")


def set_pytest_config(config):
    PytestConfig.set_config(config)
