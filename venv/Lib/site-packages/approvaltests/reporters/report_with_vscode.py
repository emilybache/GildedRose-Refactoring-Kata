from .generic_diff_reporter import GenericDiffReporter, create_config


class ReportWithVSCodeMacOS(GenericDiffReporter):
    def __init__(self):
        super().__init__(
            config=create_config(
                [
                    "ReportWithVSCode",
                    "/Applications/Visual Studio Code.app/contents/Resources/app/bin/code",
                    ["-d"],
                ]
            )
        )


class ReportWithVSCode(GenericDiffReporter):
    def __init__(self):
        super().__init__(config=create_config(["ReportWithVSCode", "code", ["-d"]]))
