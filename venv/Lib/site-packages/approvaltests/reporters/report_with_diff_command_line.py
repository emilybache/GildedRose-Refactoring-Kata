from .generic_diff_reporter import GenericDiffReporter, create_config


class ReportWithDiffCommandLine(GenericDiffReporter):
    def __init__(self):
        super().__init__(
            config=create_config(["ReportWithDiffCommandLine", "diff", ["-u"]])
        )
