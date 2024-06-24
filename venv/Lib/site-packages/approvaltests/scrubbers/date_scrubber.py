from approvaltests.scrubbers import create_regex_scrubber
from approvaltests.scrubbers.scrubbers import Scrubber


class DateScrubber:
    @staticmethod
    def get_supported_formats():
        return [
            (
                "[a-zA-Z]{3} [a-zA-Z]{3} \\d{2} \\d{2}:\\d{2}:\\d{2}",
                ["Tue May 13 16:30:00"],
            ),
            (
                "[a-zA-Z]{3} [a-zA-Z]{3} \\d{2} \\d{2}:\\d{2}:\\d{2} [a-zA-Z]{3,4} \\d{4}",
                ["Wed Nov 17 22:28:33 EET 2021"],
            ),
            (
                "[a-zA-Z]{3} [a-zA-Z]{3} \\d{2} \\d{4} \\d{2}:\\d{2}:\\d{2}.\\d{3}",
                ["Tue May 13 2014 23:30:00.789"],
            ),
            (
                "[a-zA-Z]{3} [a-zA-Z]{3} \\d{2} \\d{2}:\\d{2}:\\d{2} -\\d{4} \\d{4}",
                ["Tue May 13 16:30:00 -0800 2014"],
            ),
            (
                "\\d{2} [a-zA-Z]{3} \\d{4} \\d{2}:\\d{2}:\\d{2},\\d{3}",
                ["13 May 2014 23:50:49,999"],
            ),
            (
                "[a-zA-Z]{3} \\d{2}, \\d{4} \\d{2}:\\d{2}:\\d{2} [a-zA-Z]{2} [a-zA-Z]{3}",
                ["May 13, 2014 11:30:00 PM PST"],
            ),
            ("\\d{2}:\\d{2}:\\d{2}", ["23:30:00"]),
            (
                "\\d{4}/\\d{2}/\\d{2} \\d{2}:\\d{2}:\\d{2}.\\d{2}\\d",
                ["2014/05/13 16:30:59.786"],
            ),
            (
                "\\d{4}-\\d{1,2}-\\d{1,2}T\\d{1,2}:\\d{2}Z",
                [
                    "2020-9-10T08:07Z",
                    "2020-09-9T08:07Z",
                    "2020-09-10T8:07Z",
                    "2020-09-10T08:07Z",
                ],
            ),
            (
                "\\d{4}-\\d{1,2}-\\d{1,2}T\\d{1,2}:\\d{2}:\\d{2}Z",
                ["2020-09-10T08:07:89Z"],
            ),
            (
                "\\d{4}-\\d{1,2}-\\d{1,2}T\\d{1,2}:\\d{2}\\:\\d{2}\\.\\d{3}Z",
                ["2020-09-10T01:23:45.678Z"],
            ),
            (
                "\\d{4}-\\d{1,2}-\\d{1,2} \\d{1,2}:\\d{2}\\:\\d{2}\\.\\d{6}",
                ["2023-07-16 17:39:03.293919"],
            ),
            ("\\d{8}T\\d{6}Z", ["20210505T091112Z"]),
        ]

    def __init__(self, date_regex: str):
        self.date_regex = date_regex

    def scrub(self, date_str: str):
        return create_regex_scrubber(self.date_regex, lambda t: f"<date{t}>")(date_str)

    @staticmethod
    def get_scrubber_for(example: str) -> Scrubber:
        supported = ""
        for date_regex, examples in DateScrubber.get_supported_formats():
            supported += f"    {examples[0]} | {date_regex} \n"
            scrubber = DateScrubber(date_regex)
            if scrubber.scrub(example) == "<date0>":
                return scrubber.scrub

        raise Exception(
            f"No match found for '{example}'.\n Feel free to add your date at https://github.com/approvals/ApprovalTests.Python/issues/124 \n Current supported formats are: \n{supported}"
        )
