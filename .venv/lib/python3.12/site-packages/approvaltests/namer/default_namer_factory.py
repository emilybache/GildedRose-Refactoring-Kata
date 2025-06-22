import os
from typing import Any, Callable, Optional

from approvaltests.core.options import Options
from approvaltests.namer.default_name import get_default_namer


def is_ci(environment_loader: Callable[[str], Optional[str]] = os.environ.get) -> bool:
    possible = [
        "CI",
        "CONTINUOUS_INTEGRATION",
        "GITHUB_ACTIONS",
        "GO_SERVER_URL",
        "JENKINS_URL",
        "TF_BUILD",
    ]
    return is_team_city(environment_loader) or any(
        environment_loader(possibile_ci) for possibile_ci in possible
    )


def is_team_city(
    environment_loader: Callable[[str], Optional[str]] = os.environ.get,
) -> bool:
    team_city_version = environment_loader("TEAMCITY_VERSION")
    team_city = team_city_version and team_city_version != "LOCAL"
    return team_city


class NamerFactory:
    @staticmethod
    def with_parameters(*args: Any) -> Options:
        from approvaltests.core.scenario_namer import ScenarioNamer

        namer = ScenarioNamer(get_default_namer(), *args)
        return Options().with_namer(namer)

    @staticmethod
    def as_ci_specific_test() -> Options:
        if is_ci():
            return NamerFactory.with_parameters("ci")
        else:
            return Options()
