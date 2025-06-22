# Copyright 2015 Yelp
# Copyright 2017 Yelp
# Copyright 2018 Yelp
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
"""An example of how to parse non-line-based data.

Map +1 (U.S. and Canadian) phone numbers to the most plausible
URL for their webpage.

This is similar to the article "Analyzing the Web for the Price of a Sandwich"
(https://engineeringblog.yelp.com/2015/03/analyzing-the-web-for-the-price-of-a-sandwich.html)
except that it doesn't include Yelp biz IDs, and it doesn't need to access
S3 because it can read the input files directly.

Sample command line:

.. code-block:: sh

   python mr_phone_to_url.py -r emr --bootstrap 'sudo pip install warcio' --output-dir s3://your-bucket/path/ --no-output s3://commoncrawl/crawl-data/CC-MAIN-2018-09/segments/*/wet/*.wet.gz


To find the latest crawl:

``aws s3 ls s3://commoncrawl/crawl-data/ | grep CC-MAIN``

WET data is often added after a release; usually the second-most recent
release is a safe bet.
"""
import re
from itertools import islice

from mrjob.job import MRJob
from mrjob.py2 import urlparse
from mrjob.step import MRStep

PHONE_RE = re.compile(
    br'(?:[\D\b]|^)(1?[2-9]\d{2}[\-. ()+]+\d{3}[\-. ()+]+\d{4})(?:[\D\b]|$)')
PHONE_SEP_RE = re.compile(br'[\-. ()+]')

# hosts with more than this many phone numbers are assumed to be directories
MAX_PHONES_PER_HOST = 1000


def standardize_phone_number(number):
    """put *number* in a standard format, and convert it to a :py:class:`str`.
    """
    number_sep = PHONE_SEP_RE.split(number)
    number = b''.join(number_sep).decode('ascii')
    if len(number) > 7:
        if number[-1] not in '0123456789':
            number = number[:-1]
        if number[0] not in '0123456789':
            number = number[1:]
    if len(number) <= 10:
        return "+1" + number
    else:
        return "+" + number


class MRPhoneToURL(MRJob):
    """Use Common Crawl .wet files to map from phone number to the most
    likely URL."""

    def steps(self):
        return [
            MRStep(mapper_raw=self.extract_phone_and_url_mapper,
                   reducer=self.count_by_host_reducer),
            MRStep(reducer=self.pick_best_url_reducer),
        ]

    def extract_phone_and_url_mapper(self, wet_path, wet_uri):
        """Read in .wet file, and extract phone ant URL
        """
        from warcio.archiveiterator import ArchiveIterator

        with open(wet_path, 'rb') as f:
            for record in ArchiveIterator(f):
                if record.rec_type != 'conversion':
                    continue

                headers = record.rec_headers
                if headers.get_header('Content-Type') != 'text/plain':
                    continue

                url = headers.get_header('WARC-Target-URI')
                if not url:
                    continue

                host = urlparse(url).netloc

                payload = record.content_stream().read()
                for phone in PHONE_RE.findall(payload):
                    phone = standardize_phone_number(phone)
                    yield host, (phone, url)

    def count_by_host_reducer(self, host, phone_urls):
        phone_urls = list(islice(phone_urls, MAX_PHONES_PER_HOST + 1))

        # don't bother with directories, etc.
        host_phone_count = len(phone_urls)
        if host_phone_count > MAX_PHONES_PER_HOST:
            return

        for phone, url in phone_urls:
            yield phone, (url, host_phone_count)

    def pick_best_url_reducer(self, phone, urls_with_count):
        # pick the url that appears on a host with the least number of
        # phone numbers, breaking ties by choosing the shortest URL
        # and the one that comes first alphabetically
        urls_with_count = sorted(
            urls_with_count, key=lambda uc: (uc[1], -len(uc[0]), uc[0]))

        yield phone, urls_with_count[0][0]


if __name__ == '__main__':
    MRPhoneToURL.run()
