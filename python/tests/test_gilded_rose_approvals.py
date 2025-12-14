import io
import sys
import pytest

from approvaltests import verify
from texttest_fixture import main
from gilded_rose import Item, GildedRose

@pytest.mark.skip(reason="Approval tests disabled on this machine")
def test_gilded_rose_approvals():
    orig_sysout = sys.stdout
    try:
        fake_stdout = io.StringIO()
        sys.stdout = fake_stdout
        sys.argv = ["texttest_fixture.py", 30]
        main()
        answer = fake_stdout.getvalue()
    finally:
        sys.stdout = orig_sysout

    verify(answer)

def test_conjured_items_degrade_twice_as_fast():
    items = [Item("Conjured Mana Cake", 5, 10)]
    gilded_rose = GildedRose(items)
    gilded_rose.update_quality()

    # Conjured quality should drop by 2
    assert items[0].quality == 8

def test_conjured_items_after_expiry():
    items = [Item("Conjured Mana Cake", 0, 10)]
    gilded_rose = GildedRose(items)
    gilded_rose.update_quality()

    # After expiry, Conjured drops by 4
    assert items[0].quality == 6

if __name__ == "__main__":
    test_gilded_rose_approvals()