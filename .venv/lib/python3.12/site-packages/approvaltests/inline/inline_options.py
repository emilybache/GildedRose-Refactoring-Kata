from __future__ import annotations

from pathlib import Path
from typing import TYPE_CHECKING

from typing_extensions import override

if TYPE_CHECKING:
    from approvaltests.core.options import Options

PREVIOUS_RESULT_ = "vvvvv PREVIOUS RESULT vvvvv\n"

DELETE_ME_TO_APPROVE_ = "\n***** DELETE ME TO APPROVE *****"


class InlineOptions:

    @staticmethod
    def automatic() -> "InlineOptions":
        from approvaltests.namer.inline_python_reporter import InlinePythonReporter
        from approvaltests.reporters import ReporterThatAutomaticallyApproves

        class AutomaticInlineOptions(InlineOptions):
            @override
            def apply(self, options: "Options") -> "Options":
                return options.with_reporter(
                    InlinePythonReporter(ReporterThatAutomaticallyApproves())
                )

        return AutomaticInlineOptions()

    @staticmethod
    def semi_automatic() -> "InlineOptions":
        from approvaltests.namer.inline_python_reporter import InlinePythonReporter
        from approvaltests.reporters import ReporterThatAutomaticallyApproves

        class SemiAutomaticInlineOptions(InlineOptions):
            @override
            def apply(self, options: "Options") -> "Options":
                return options.with_reporter(
                    InlinePythonReporter(
                        ReporterThatAutomaticallyApproves(),
                        create_footer_function=lambda __, ___: DELETE_ME_TO_APPROVE_,
                    )
                )

        return SemiAutomaticInlineOptions()

    @staticmethod
    def semi_automatic_with_previous_approved() -> "InlineOptions":
        from approvaltests.namer.inline_python_reporter import InlinePythonReporter
        from approvaltests.reporters import ReporterThatAutomaticallyApproves

        def create_previous_capture_footer(
            received_path: str, approved_path: str
        ) -> str:
            approved_text = Path(approved_path).read_text()
            approved_text = approved_text.rsplit("\n", 1)[0]
            approved_text = approved_text.rsplit(PREVIOUS_RESULT_, 1)[-1]
            received_text = Path(received_path).read_text().rsplit("\n", 1)[0]

            if received_text != approved_text:
                return DELETE_ME_TO_APPROVE_ + "\n" + PREVIOUS_RESULT_ + approved_text
            return ""

        class PreviousCaptureInlineOptions(InlineOptions):
            @override
            def apply(self, options: "Options") -> "Options":
                return options.with_reporter(
                    InlinePythonReporter(
                        ReporterThatAutomaticallyApproves(),
                        create_footer_function=create_previous_capture_footer,
                    )
                )

        return PreviousCaptureInlineOptions()

    def apply(self, options: "Options") -> "Options":

        return options

    @staticmethod
    def show_code(do_show_code: bool = True) -> InlineOptions:
        from approvaltests.namer.inline_python_reporter import InlinePythonReporter

        class ShowCodeInlineOptions(InlineOptions):
            @override
            def apply(self, options: "Options") -> "Options":
                return options.with_reporter(InlinePythonReporter(options.reporter))

        class DoNotShowCodeInlineOptions(InlineOptions):
            @override
            def apply(self, options: "Options") -> "Options":
                return options

        return ShowCodeInlineOptions() if do_show_code else DoNotShowCodeInlineOptions()
