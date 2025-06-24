import io
import sys
import os
sys.path.insert(0, os.path.abspath(os.path.dirname(__file__) + '/../'))

from approvaltests import verify
from texttest_fixture import main

def test_gilded_rose_approvals():
    fake_stdout = io.StringIO()
    orig_sysout = sys.stdout
    try:
        sys.stdout = fake_stdout
        sys.argv = ["texttest_fixture.py", "30"]
        main()
    finally:
        sys.stdout = orig_sysout
    answer = fake_stdout.getvalue()
    verify(answer)

if __name__ == "__main__":
    test_gilded_rose_approvals()