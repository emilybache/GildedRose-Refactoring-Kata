"""Approvaltests.scrubbers module."""

from .scrubbers import (
    create_regex_scrubber,
    scrub_all_dates,
    scrub_all_guids,
    combine_scrubbers,
    templates_regex_scrubber_with_lambda,
    templates_regex_scrubber_with_replacement,
)
