# Copyright 2009-2010 Yelp
# Copyright 2013 David Marin
# Copyright 2017 Yelp
# Copyright 2019 Yelp
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
"""A text classifier that uses a modified version of Naive Bayes that is not
sensitive to document length.

This is a somewhat contrived example in that it does everything in one job;
generally you'd run one job to generate n-gram scores, put them in a sqlite
database, and run a second job to score documents. But this is simple, and
it works!

This takes text files as its input. Each file's name should have the format
```unique_id-cat_id_1-cat_id_2-etc.txt```, where ```unique_id``` is unique
for this document, and ```cat_id_1``` etc. are categories the document
is known to belong to. You may also use the form ```not_cat_id_3``` to
indicate that a document is known *not* to belong to ```cat_id_3```. Document
and category IDs may not contain the ```-``` or ```.``` characters. For
examples, look in ```docs-to-classify/```.

This job outputs the documents, with the field 'cat_to_score' filled in.
Generally, positive scores indicate the document is in the category, and
negative scores indicate it is not, but it's up to you to determine a
threshold for each category. This job also outputs scores for each ngram,
so that you can classify other documents.

About half of the documents are placed in a test set (based on SHA1 hash of
their text), which means they will not be used to train the classifier. The
'in_test_set' of each document will be filled accordingly. You can turn
this off with the --no-test-set flag. (You can also effectively put docs
in the training set by specifying no category information.)

Some terminology:

An "ngram" is a word or phrase. "foo" is a 1-gram; "foo bar baz" is a 3-gram.

"tf" refers to term frequency, that is, the number of times an ngram appears.
"df" referse to document frequency, that is, the number of documents an ngram
appears in at least once.
"""
from collections import defaultdict
import hashlib
import math
import posixpath
import re

from mrjob.job import MRJob
from mrjob.py2 import to_unicode
from mrjob.step import MRStep
from mrjob.util import file_ext


def parse_doc_filename(input_uri):
    """Parse a filename like ``some_id-cat1-cat2-not_cat3.txt`` into
    ``dict(id='some_id', cats=dict(cat1=True, cat2=True, cat3=False))``
    """
    # get filename without extension
    name_with_ext = posixpath.basename(input_uri)
    name = name_with_ext[:-len(file_ext(name_with_ext))]

    parts = name.split('-')

    doc_id = parts[0]
    cats = {}

    for part in parts[1:]:
        if part.startswith('not_'):
            cats[part[4:]] = False
        else:
            cats[part] = True

    return dict(id=doc_id, cats=cats)


def count_ngrams(text, max_ngram_size, stop_words):
    """Break text down into ngrams, and return a dictionary mapping
    (n, ngram) to number of times that ngram occurs.

    n: ngram size ("foo" is a 1-gram, "foo bar baz" is a 3-gram)
    ngram: the ngram, as a space-separated string or None to indicate the
        ANY ngram (basically the number of words in the document).

    Args:
    text -- text, as a unicode
    max_ngram_size -- maximum size of ngrams to consider
    stop_words -- a collection of words (in lowercase) to remove before
        parsing out ngrams (e.g. "the", "and")
    """
    if not isinstance(stop_words, set):
        stop_words = set(stop_words)

    words = [word.lower() for word in WORD_RE.findall(text)
             if word.lower() not in stop_words]

    ngram_counts = defaultdict(int)

    for i in range(len(words)):
        for n in range(1, max_ngram_size + 1):
            if i + n <= len(words):
                ngram = ' '.join(words[i:i + n])
                ngram_counts[(n, ngram)] += 1

    # add counts for ANY ngram
    for n in range(1, max_ngram_size + 1):
        ngram_counts[(n, None)] = len(words) - n + 1

    return ngram_counts


WORD_RE = re.compile(r"[\w']+", re.UNICODE)

DEFAULT_MAX_NGRAM_SIZE = 4

DEFAULT_STOP_WORDS = [
    'a', 'about', 'also', 'am', 'an', 'and', 'any', 'are', 'as', 'at', 'be',
    'but', 'by', 'can', 'com', 'did', 'do', 'does', 'for', 'from', 'had',
    'has', 'have', 'he', "he'd", "he'll", "he's", 'her', 'here', 'hers',
    'him', 'his', 'i', "i'd", "i'll", "i'm", "i've", 'if', 'in', 'into', 'is',
    'it', "it's", 'its', 'just', 'me', 'mine', 'my', 'of', 'on', 'or', 'org',
    'our', 'ours', 'she', "she'd", "she'll", "she's", 'some', 'than', 'that',
    'the', 'their', 'them', 'then', 'there', 'these', 'they', "they'd",
    "they'll", "they're", 'this', 'those', 'to', 'us', 'was', 'we', "we'd",
    "we'll", "we're", 'were', 'what', 'where', 'which', 'who', 'will', 'with',
    'would', 'you', 'your', 'yours',
]


class MRTextClassifier(MRJob):

    def steps(self):
        """Conceptually, the steps are:
        1. Parse documents into ngrams
        2. Group by ngram to get a frequency count for each ngram, and to
           exclude very rare ngrams
        3. Send all ngram information to one "global" reducer so we can
           assign scores for each category and ngram
        4. Group scores and documents by ngram and compute score for that
           ngram for that document. Exclude very common ngrams to save memory.
        5. Average together scores for each document to get its score for
           each category.

        The documents themselves are passed through from step 1 to step 5.
        Ngram scoring information is passed through from step 4 to step 5.
        """
        return [MRStep(mapper_raw=self.parse_doc,
                       reducer=self.count_ngram_freq),
                MRStep(reducer=self.score_ngrams),
                MRStep(reducer=self.score_documents_by_ngram),
                MRStep(reducer=self.score_documents)]

    def configure_args(self):
        """Add command-line options specific to this script."""
        super(MRTextClassifier, self).configure_args()

        self.add_passthru_arg(
            '--min-df', dest='min_df', default=2, type=int,
            help=('min number of documents an n-gram must appear in for us to'
                  ' count it. Default: %(default)s'))
        self.add_passthru_arg(
            '--max-df', dest='max_df', default=10000000, type=int,
            help=('max number of documents an n-gram may appear in for us to'
                  ' count it (this keeps reducers from running out of memory).'
                  ' Default: %(default)s'))
        self.add_passthru_arg(
            '--max-ngram-size', dest='max_ngram_size',
            default=DEFAULT_MAX_NGRAM_SIZE, type=int,
            help='maximum phrase length to consider')
        self.add_passthru_arg(
            '--stop-words', dest='stop_words',
            default=', '.join(DEFAULT_STOP_WORDS),
            help=("comma-separated list of words to ignore. For example, "
                  "--stop-words 'in, the' would cause 'hole in the wall' to be"
                  " parsed as ['hole', 'wall']. Default: %(default)s"))
        self.add_passthru_arg(
            '--short-doc-threshold', dest='short_doc_threshold',
            type=int, default=None,
            help=('Normally, for each n-gram size, we take the average score'
                  ' over all n-grams that appear. This allows us to penalize'
                  ' short documents by using this threshold as the denominator'
                  ' rather than the actual number of n-grams.'))
        self.add_passthru_arg(
            '--no-test-set', dest='no_test_set',
            action='store_true', default=False,
            help=("Choose about half of the documents to be the testing set"
                  " (don't use them to train the classifier) based on a SHA1"
                  " hash of their text"))

    def load_args(self, args):
        """Parse stop_words option."""
        super(MRTextClassifier, self).load_args(args)

        self.stop_words = set()
        if self.options.stop_words:
            self.stop_words.update(
                s.strip().lower() for s in self.options.stop_words.split(','))

    def parse_doc(self, input_path, input_uri):
        """Mapper: parse documents and emit ngram information.

        Input: Text files whose name contains a unique document ID and
        category information (see :py:func:`parse_doc_filename`).

        Output:
        ``('ngram', (n, ngram)), (count, cats)`` OR
        ``('doc', doc_id), doc``

        n: ngram length
        ngram: ngram encoded encoded as a string (e.g. "pad thai")
            or None to indicate ANY ngram.
        count:  # of times an ngram appears in the document
        cats: a map from category name to a boolean indicating whether it's
            this document is in the category

        doc_id: (hopefully) unique document ID
        doc: the encoded document. We'll fill these fields:
            ngram_counts: map from (n, ngram) to  # of times ngram appears
                in the document, using (n, None) to represent the total
                number of times ANY ngram of that size appears (essentially
                number of words)
            in_test_set: boolean indicating if this doc is in the test set
            id: SHA1 hash of doc text (if not already filled)
        """
        # fill *id* and *cats*
        doc = parse_doc_filename(input_uri)

        with open(input_path) as f:
            text = to_unicode(f.read())

        # pick test/training docs
        if self.options.no_test_set:
            doc['in_test_set'] = False
        else:
            doc_hash = hashlib.sha1(text.encode('utf-8')).hexdigest()
            doc['in_test_set'] = bool(int(doc_hash[-1], 16) % 2)

        # map from (n, ngram) to number of times it appears
        ngram_counts = count_ngrams(
            text, self.options.max_ngram_size, self.stop_words)

        # yield the number of times the ngram appears in this doc
        # and the categories for this document, so we can train the classifier
        if not doc['in_test_set']:
            for (n, ngram), count in ngram_counts.items():
                yield ('ngram', (n, ngram)), (count, doc['cats'])

        # yield the document itself, for safekeeping
        doc['ngram_counts'] = list(ngram_counts.items())
        yield ('doc', doc['id']), doc

    def count_ngram_freq(self, type_and_key, values):
        """Reducer: Combine information about how many times each ngram
        appears for docs in/not in each category. Dump ngrams that appear
        in very few documents (according to --min-df switch). If two documents
        have the same ID, increment a counter and only keep one; otherwise
        pass docs through unchanged.

        Input (see parse_doc() for details):
        ('ngram', (n, ngram)), (count, cats) OR
        ('doc', doc_id), doc

        Output:
        ('global', None), ((n, ngram), (cat_to_df, cat_to_tf)) OR
        ('doc', doc_id), doc
        n: ngram length
        ngram: ngram encoded encoded as a string (e.g. "pad thai")
            or None to indicate ANY ngram.
        cat_to_df: list of tuples of ((cat_name, is_in_category), df); df
            is  # of documents of this type that the ngram appears in
        cat_to_tf: list of tuples of ((cat_name, is_in_category), df); tf
            is  # of time the ngram appears in docs of this type
        doc_id: unique document ID
        doc: the encoded document
        """
        key_type, key = type_and_key

        # pass documents through
        if key_type == 'doc':
            doc_id = key
            docs = list(values)
            # if two documents end up with the same key, only keep one
            if len(docs) > 1:
                self.increment_counter(
                    'Document key collision', str(doc_id))
            yield ('doc', doc_id), docs[0]
            return

        assert key_type == 'ngram'
        n, ngram = key

        # total # of docs this ngram appears in
        total_df = 0
        # map from (cat, is_in_cat) to
        # number of documents in this cat it appears in (df), or
        # number of times it appears in documents of this type (tf)
        cat_to_df = defaultdict(int)
        cat_to_tf = defaultdict(int)

        for count, cats in values:
            total_df += 1
            for cat in cats.items():
                cat_to_df[cat] += 1
                cat_to_tf[cat] += count

        # don't bother with very rare ngrams
        if total_df < self.options.min_df:
            return

        yield (('global', None),
               ((n, ngram),
                (list(cat_to_df.items()), list(cat_to_tf.items()))))

    def score_ngrams(self, type_and_key, values):
        """Reducer: Look at all ngrams together, and assign scores by
        ngram and category. Also farm out documents to the reducer for
        any ngram they contain, and pass documents through to the next
        step.

        To score an ngram for a category, we compare the probability of any
        given ngram being our ngram for documents in the category against
        documents not in the category. The score is just the log of the
        ratio of probabilities (the "log difference")

        Input (see count_ngram_freq() for details):
        ('global', None), ((n, ngram), (cat_to_df, cat_to_tf)) OR
        ('doc', doc_id), doc

        Output:
        ('doc', doc_id), document OR
        ('ngram', (n, ngram)), ('doc_id', doc_id) OR
        ('ngram', (n, ngram)), ('cat_to_score', cat_to_score)

        n: ngram length
        ngram: ngram encoded encoded as a string (e.g. "pad thai")
            or None to indicate ANY ngram.
        cat_to_score: map from (cat_name, is_in_category) to score for
            this ngram
        doc_id: unique document ID
        doc: the encoded document
        """
        key_type, key = type_and_key
        if key_type == 'doc':
            doc_id = key
            doc = list(values)[0]
            # pass document through
            yield ('doc', doc_id), doc

            # send document to reducer for every ngram it contains
            for (n, ngram), count in doc['ngram_counts']:
                # don't bother even creating a reducer for the ANY ngram
                # because we'd have to send all documents to it.
                if ngram is None:
                    continue
                yield (('ngram', (n, ngram)),
                       ('doc_id', doc_id))

            return

        assert key_type == 'global'
        ngram_to_info = dict(
            ((n, ngram),
             (dict((tuple(cat), df) for cat, df in cat_to_df),
              dict((tuple(cat), tf) for cat, tf in cat_to_tf)))
            for (n, ngram), (cat_to_df, cat_to_tf)
            in values)

        # m = # of possible ngrams of any given type. This is not a very
        # rigorous estimate, but it's good enough
        m = len(ngram_to_info)

        for (n, ngram), info in ngram_to_info.items():
            # do this even for the special ANY ngram; it's useful
            # as a normalization factor.
            cat_to_df, cat_to_tf = info

            # get the total # of documents and terms for ngrams of this size
            cat_to_d, cat_to_t = ngram_to_info[(n, None)]

            # calculate the probability of any given term being
            # this term for documents of each type
            cat_to_p = {}
            for cat, t in cat_to_t.items():
                tf = cat_to_tf.get(cat) or 0
                # use Laplace's rule of succession to estimate p. See:
                # http://en.wikipedia.org/wiki/Rule_of_succession#Generalization_to_any_number_of_possibilities
                cat_to_p[cat] = (tf + (2.0 / m)) / (t + 2)

            cats = set(cat for cat, in_cat in cat_to_t)
            cat_to_score = {}
            for cat in cats:
                p_if_in = cat_to_p.get((cat, True), 1.0 / m)
                p_if_out = cat_to_p.get((cat, False), 1.0 / m)
                # take the log difference of probabilities
                score = math.log(p_if_in) - math.log(p_if_out)
                cat_to_score[cat] = score

            yield (('ngram', (n, ngram)),
                   ('cat_to_score', cat_to_score))

    def score_documents_by_ngram(self, type_and_key, types_and_values):
        """Reducer: For all documents that contain a given ngram, send
        scoring info to that document. Also pass documents and scoring
        info through as-is

        Input (see score_ngrams() for details):
        ('doc', doc_id), doc OR
        ('ngram', (n, ngram)), ('doc_id', doc_id) OR
        ('ngram', (n, ngram)), ('cat_to_score', cat_to_score)

        Output:
        ('doc', doc_id), ('doc', doc)
        ('doc', doc_id), ('scores', ((n, ngram), cat_to_score))
        ('cat_to_score', (n, ngram)), cat_to_score

        n: ngram length
        ngram: ngram encoded encoded as a string (e.g. "pad thai")
            or None to indicate ANY ngram.
        cat_to_score: map from (cat_name, is_in_category) to score for
            this ngram
        doc_id: unique document ID
        doc: the encoded document
        """
        key_type, key = type_and_key

        # pass documents through
        if key_type == 'doc':
            doc_id = key
            doc = list(types_and_values)[0]
            yield ('doc', doc_id), ('doc', doc)
            return

        assert key_type == 'ngram'
        n, ngram = key

        doc_ids = []
        cat_to_score = None

        for value_type, value in types_and_values:
            if value_type == 'cat_to_score':
                cat_to_score = value
                continue

            assert value_type == 'doc_id'
            doc_ids.append(value)

            if len(doc_ids) > self.options.max_df:
                self.increment_counter('Exceeded max df', repr((n, ngram)))
                return

        # skip ngrams that are too rare to score
        if cat_to_score is None:
            return

        # send score info for this ngram to this document
        for doc_id in doc_ids:
            yield ('doc', doc_id), ('scores', ((n, ngram), cat_to_score))

        # keep scoring info
        yield ('cat_to_score', (n, ngram)), cat_to_score

    def score_documents(self, type_and_key, types_and_values):
        """Reducer: combine all scoring information for each document, and
        add it to the document. Also pass ngram scores through as-is.

        To score a document, we essentially take a weighted average of all
        the scores for ngrams of each size, and then sum together those
        averages. ngrams that aren't scored (because they're very rare or
        very common) are considered to have a score of zero. Using averages
        allows us to be insensitive to document size. There is a penalty
        for very small documents.

        Input (see score_ngrams() for details):
        ('doc', doc_id), ('doc', doc)
        ('doc', doc_id), ('scores', ((n, ngram), cat_to_score))
        ('cat_to_score', (n, ngram)), cat_to_score

        Output:
        ('doc', doc_id), doc
        ('cat_to_score', (n, ngram)), cat_to_score

        n: ngram length
        ngram: ngram encoded encoded as a string (e.g. "pad thai")
            or None to indicate ANY ngram.
        cat_to_score: map from (cat_name, is_in_category) to score for
            this ngram
        doc_id: unique document ID
        doc: the encoded document. this will contain an extra field
            'cat_to_score', and will no longer have the 'ngram_counts' field.
        """
        key_type, key = type_and_key

        # pass through cat_to_score
        if key_type == 'cat_to_score':
            cat_to_score = list(types_and_values)[0]
            yield ('cat_to_score', key), cat_to_score
            return

        assert key_type == 'doc'
        doc_id = key

        # store the document and scoring info
        doc = None
        ngrams_and_scores = []

        for value_type, value in types_and_values:
            if value_type == 'doc':
                doc = value
                continue

            assert value_type == 'scores'
            ((n, ngram), cat_to_score) = value
            ngrams_and_scores.append(((n, ngram), cat_to_score))

        # total scores for each ngram size
        ngram_counts = dict(((n, ngram), count)
                            for (n, ngram), count in doc['ngram_counts'])

        cat_to_n_to_total_score = defaultdict(lambda: defaultdict(float))

        for (n, ngram), cat_to_score in ngrams_and_scores:
            tf = ngram_counts[(n, ngram)]
            for cat, score in cat_to_score.items():
                cat_to_n_to_total_score[cat][n] += score * tf

        # average scores for each ngram size
        cat_to_score = {}
        for cat, n_to_total_score in cat_to_n_to_total_score.items():
            total_score_for_cat = 0
            for n, total_score in n_to_total_score.items():
                total_t = ngram_counts[(n, None)]
                total_score_for_cat += (
                    total_score /
                    max(total_t, self.options.short_doc_threshold or 0, 1))
            cat_to_score[cat] = total_score_for_cat

        # add scores to the document, and get rid of ngram_counts
        doc['cat_to_score'] = cat_to_score
        del doc['ngram_counts']

        yield ('doc', doc_id), doc


if __name__ == '__main__':
    MRTextClassifier.run()
