# GildedRose Strategy Pattern Refactor — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Refactor `GildedRose.update_quality` from a nested if/elif chain into a Strategy Pattern with an internal `ItemRecord` dataclass, `days` injection, and full test coverage including the new Conjured item type.

**Architecture:** Each item type gets a dedicated `UpdateStrategy` class. `GildedRose.__init__` wraps every `Item` in an internal `ItemRecord(item, strategy)` by looking up the item name in a dict; unknowns get `NormalStrategy`. `update_quality(days=1)` delegates to each record's strategy, simulating `days` days in one call.

**Tech Stack:** Python 3.10+, `unittest.TestCase`, `pytest`, `dataclasses`, `typing.Protocol`

---

## File Map

| File | Change |
|---|---|
| `gilded_rose.py` | Add `UpdateStrategy` Protocol, five strategy classes, `ItemRecord` dataclass, `_STRATEGY_MAP`, refactor `GildedRose` |
| `tests/test_gilded_rose.py` | Fix `test_foo`, add one `TestCase` per strategy class + integration `TestGildedRose` |

---

### Task 1: Fix the broken placeholder test

**Files:**
- Modify: `tests/test_gilded_rose.py`

The existing `test_foo` always fails because it asserts `items[0].name == "fixme"` — `update_quality` never changes names. Fix it to assert the real behaviour: a normal item with `quality=0` stays at `0` after one update.

- [ ] **Step 1: Update the assertion and rename the test**

Replace the test body in `tests/test_gilded_rose.py`:

```python
class GildedRoseTest(unittest.TestCase):
    def test_normal_item_at_zero_quality_stays_zero(self):
        items = [Item("foo", 0, 0)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(0, items[0].quality)
```

- [ ] **Step 2: Run the test and confirm it passes**

```bash
cd /Users/rajvora/Documents/interviews/GildedRose-Refactoring-Kata/python
python3 -m pytest tests/test_gilded_rose.py -v
```

Expected: `PASSED tests/test_gilded_rose.py::GildedRoseTest::test_normal_item_at_zero_quality_stays_zero`

- [ ] **Step 3: Commit**

```bash
git add tests/test_gilded_rose.py
git commit -m "fix: correct placeholder test_foo assertion"
```

---

### Task 2: NormalStrategy — TDD

**Files:**
- Modify: `tests/test_gilded_rose.py` — add `TestNormalStrategy`
- Modify: `gilded_rose.py` — add `NormalStrategy`

Normal items: `-1 quality/day`, `-2/day` once `sell_in < 0`, quality floors at `0`.

- [ ] **Step 1: Write failing tests for NormalStrategy**

Add this class to `tests/test_gilded_rose.py` (after the existing `GildedRoseTest`):

```python
class TestNormalStrategy(unittest.TestCase):
    """Tests for NormalStrategy — default degradation behaviour."""

    def setUp(self):
        self.strategy = NormalStrategy()

    def test_quality_decrements_by_one_each_day(self):
        item = Item("widget", sell_in=10, quality=20)
        self.strategy.update(item, days=1)
        self.assertEqual(19, item.quality)
        self.assertEqual(9, item.sell_in)

    def test_quality_floors_at_zero(self):
        # Quality must never go negative
        item = Item("widget", sell_in=5, quality=0)
        self.strategy.update(item, days=1)
        self.assertEqual(0, item.quality)

    def test_quality_degrades_twice_after_sell_date(self):
        # Once sell_in goes below 0, each day removes 2 quality
        item = Item("widget", sell_in=0, quality=10)
        self.strategy.update(item, days=1)
        self.assertEqual(8, item.quality)
        self.assertEqual(-1, item.sell_in)

    def test_multi_day_crosses_sell_date(self):
        # Days=5 starting from sell_in=2: 2 normal days then 3 post-sell days
        # Day1: q=9 si=1 | Day2: q=8 si=0 | Day3: q=6 si=-1
        # Day4: q=4 si=-2 | Day5: q=2 si=-3
        item = Item("widget", sell_in=2, quality=10)
        self.strategy.update(item, days=5)
        self.assertEqual(2, item.quality)
        self.assertEqual(-3, item.sell_in)

    def test_quality_floors_at_zero_when_crossing_sell_date(self):
        item = Item("widget", sell_in=1, quality=1)
        # Day1: q=0 si=0 | Day2: post-sell but already 0
        self.strategy.update(item, days=2)
        self.assertEqual(0, item.quality)
```

Also update the import line at the top of the test file to include `NormalStrategy`:

```python
from gilded_rose import Item, GildedRose, NormalStrategy
```

- [ ] **Step 2: Run tests to confirm they FAIL**

```bash
python3 -m pytest tests/test_gilded_rose.py::TestNormalStrategy -v
```

Expected: `ImportError` or `AttributeError` — `NormalStrategy` does not exist yet.

- [ ] **Step 3: Implement NormalStrategy in gilded_rose.py**

Add at the top of `gilded_rose.py` (before the `GildedRose` class, after any existing code):

```python
# -*- coding: utf-8 -*-
from dataclasses import dataclass
from typing import Protocol


class UpdateStrategy(Protocol):
    """Interface for per-item-type quality update logic."""

    def update(self, item: "Item", days: int) -> None:
        """Mutate item.quality and item.sell_in to reflect `days` passing."""
        ...


class NormalStrategy:
    """
    Default degradation strategy.

    Quality decreases by 1 per day; by 2 per day once the sell date
    has passed (sell_in < 0). Quality never falls below 0.
    """

    def update(self, item: "Item", days: int) -> None:
        for _ in range(days):
            item.quality = max(0, item.quality - 1)
            item.sell_in -= 1
            if item.sell_in < 0:
                item.quality = max(0, item.quality - 1)
```

- [ ] **Step 4: Run tests to confirm they PASS**

```bash
python3 -m pytest tests/test_gilded_rose.py::TestNormalStrategy -v
```

Expected: all 5 tests `PASSED`.

- [ ] **Step 5: Run full suite to confirm no regressions**

```bash
python3 -m pytest tests/ -v
```

Expected: all tests pass.

- [ ] **Step 6: Commit**

```bash
git add gilded_rose.py tests/test_gilded_rose.py
git commit -m "feat: add NormalStrategy with days injection"
```

---

### Task 3: AgedBrieStrategy — TDD

**Files:**
- Modify: `tests/test_gilded_rose.py` — add `TestAgedBrieStrategy`
- Modify: `gilded_rose.py` — add `AgedBrieStrategy`

Aged Brie: `+1 quality/day`, `+2/day` after sell date, caps at `50`.

- [ ] **Step 1: Write failing tests**

Add to the import line:

```python
from gilded_rose import Item, GildedRose, NormalStrategy, AgedBrieStrategy
```

Add this class:

```python
class TestAgedBrieStrategy(unittest.TestCase):
    """Tests for AgedBrieStrategy — quality improves with age."""

    def setUp(self):
        self.strategy = AgedBrieStrategy()

    def test_quality_increments_by_one_each_day(self):
        item = Item("Aged Brie", sell_in=10, quality=20)
        self.strategy.update(item, days=1)
        self.assertEqual(21, item.quality)
        self.assertEqual(9, item.sell_in)

    def test_quality_caps_at_50(self):
        item = Item("Aged Brie", sell_in=10, quality=50)
        self.strategy.update(item, days=1)
        self.assertEqual(50, item.quality)

    def test_quality_increments_twice_after_sell_date(self):
        # Past sell date: +2 per day
        item = Item("Aged Brie", sell_in=0, quality=20)
        self.strategy.update(item, days=1)
        self.assertEqual(22, item.quality)
        self.assertEqual(-1, item.sell_in)

    def test_multi_day_crosses_sell_date(self):
        # sell_in=1, quality=46, days=3
        # Day1: q=47 si=0 | Day2: q=49 si=-1 | Day3: q=50 (capped) si=-2
        item = Item("Aged Brie", sell_in=1, quality=46)
        self.strategy.update(item, days=3)
        self.assertEqual(50, item.quality)
        self.assertEqual(-2, item.sell_in)
```

- [ ] **Step 2: Run tests to confirm they FAIL**

```bash
python3 -m pytest tests/test_gilded_rose.py::TestAgedBrieStrategy -v
```

Expected: `ImportError` — `AgedBrieStrategy` does not exist yet.

- [ ] **Step 3: Implement AgedBrieStrategy in gilded_rose.py**

Add after `NormalStrategy`:

```python
class AgedBrieStrategy:
    """
    Quality improvement strategy for Aged Brie.

    Quality increases by 1 per day; by 2 per day once the sell date
    has passed. Quality never exceeds 50.
    """

    def update(self, item: "Item", days: int) -> None:
        for _ in range(days):
            item.quality = min(50, item.quality + 1)
            item.sell_in -= 1
            if item.sell_in < 0:
                item.quality = min(50, item.quality + 1)
```

- [ ] **Step 4: Run tests to confirm they PASS**

```bash
python3 -m pytest tests/test_gilded_rose.py::TestAgedBrieStrategy -v
```

Expected: all 4 tests `PASSED`.

- [ ] **Step 5: Run full suite**

```bash
python3 -m pytest tests/ -v
```

- [ ] **Step 6: Commit**

```bash
git add gilded_rose.py tests/test_gilded_rose.py
git commit -m "feat: add AgedBrieStrategy"
```

---

### Task 4: BackstagePassStrategy — TDD

**Files:**
- Modify: `tests/test_gilded_rose.py` — add `TestBackstagePassStrategy`
- Modify: `gilded_rose.py` — add `BackstagePassStrategy`

Rules: `+1` quality normally; `+2` when `sell_in ≤ 10`; `+3` when `sell_in ≤ 5`; drops to `0` when `sell_in` reaches `0` at start of day (concert day). Quality caps at `50`.

- [ ] **Step 1: Write failing tests**

Update import:

```python
from gilded_rose import (
    Item, GildedRose, NormalStrategy, AgedBrieStrategy,
    BackstagePassStrategy,
)
```

Add:

```python
class TestBackstagePassStrategy(unittest.TestCase):
    """Tests for BackstagePassStrategy — tiered increase, zeroes after concert."""

    def setUp(self):
        self.strategy = BackstagePassStrategy()

    def test_quality_increments_by_1_when_more_than_10_days(self):
        item = Item("Backstage passes to a TAFKAL80ETC concert", sell_in=15, quality=20)
        self.strategy.update(item, days=1)
        self.assertEqual(21, item.quality)
        self.assertEqual(14, item.sell_in)

    def test_quality_increments_by_2_when_10_or_fewer_days(self):
        item = Item("Backstage passes to a TAFKAL80ETC concert", sell_in=10, quality=20)
        self.strategy.update(item, days=1)
        self.assertEqual(22, item.quality)
        self.assertEqual(9, item.sell_in)

    def test_quality_increments_by_3_when_5_or_fewer_days(self):
        item = Item("Backstage passes to a TAFKAL80ETC concert", sell_in=5, quality=20)
        self.strategy.update(item, days=1)
        self.assertEqual(23, item.quality)
        self.assertEqual(4, item.sell_in)

    def test_quality_drops_to_zero_on_concert_day(self):
        # sell_in=0 means the concert is today — quality zeroes out
        item = Item("Backstage passes to a TAFKAL80ETC concert", sell_in=0, quality=30)
        self.strategy.update(item, days=1)
        self.assertEqual(0, item.quality)
        self.assertEqual(-1, item.sell_in)

    def test_quality_stays_zero_after_concert(self):
        item = Item("Backstage passes to a TAFKAL80ETC concert", sell_in=-1, quality=0)
        self.strategy.update(item, days=1)
        self.assertEqual(0, item.quality)

    def test_quality_caps_at_50(self):
        item = Item("Backstage passes to a TAFKAL80ETC concert", sell_in=5, quality=49)
        self.strategy.update(item, days=1)
        self.assertEqual(50, item.quality)

    def test_multi_day_crosses_all_thresholds(self):
        # sell_in=12, quality=20, days=4
        # Day1(si=12): +1 → q=21, si=11
        # Day2(si=11): +1 → q=22, si=10
        # Day3(si=10): +2 → q=24, si=9
        # Day4(si=9):  +2 → q=26, si=8
        item = Item("Backstage passes to a TAFKAL80ETC concert", sell_in=12, quality=20)
        self.strategy.update(item, days=4)
        self.assertEqual(26, item.quality)
        self.assertEqual(8, item.sell_in)
```

- [ ] **Step 2: Run tests to confirm they FAIL**

```bash
python3 -m pytest tests/test_gilded_rose.py::TestBackstagePassStrategy -v
```

Expected: `ImportError` — `BackstagePassStrategy` not defined.

- [ ] **Step 3: Implement BackstagePassStrategy**

Add after `AgedBrieStrategy`:

```python
class BackstagePassStrategy:
    """
    Quality strategy for backstage passes.

    Quality increases as the concert approaches:
      - +1/day when more than 10 days remain
      - +2/day when 10 or fewer days remain
      - +3/day when 5 or fewer days remain
    Quality drops to 0 on concert day (sell_in == 0 at start of day)
    and stays 0 afterwards. Quality never exceeds 50.
    """

    def update(self, item: "Item", days: int) -> None:
        for _ in range(days):
            if item.sell_in <= 0:
                # Concert day or past — quality is worthless
                item.quality = 0
                item.sell_in -= 1
                continue
            if item.sell_in <= 5:
                item.quality = min(50, item.quality + 3)
            elif item.sell_in <= 10:
                item.quality = min(50, item.quality + 2)
            else:
                item.quality = min(50, item.quality + 1)
            item.sell_in -= 1
```

- [ ] **Step 4: Run tests to confirm they PASS**

```bash
python3 -m pytest tests/test_gilded_rose.py::TestBackstagePassStrategy -v
```

Expected: all 7 tests `PASSED`.

- [ ] **Step 5: Run full suite**

```bash
python3 -m pytest tests/ -v
```

- [ ] **Step 6: Commit**

```bash
git add gilded_rose.py tests/test_gilded_rose.py
git commit -m "feat: add BackstagePassStrategy"
```

---

### Task 5: SulfurasStrategy — TDD

**Files:**
- Modify: `tests/test_gilded_rose.py` — add `TestSulfurasStrategy`
- Modify: `gilded_rose.py` — add `SulfurasStrategy`

Sulfuras is legendary: `quality` is always `80`, `sell_in` never changes.

- [ ] **Step 1: Write failing tests**

Update import:

```python
from gilded_rose import (
    Item, GildedRose, NormalStrategy, AgedBrieStrategy,
    BackstagePassStrategy, SulfurasStrategy,
)
```

Add:

```python
class TestSulfurasStrategy(unittest.TestCase):
    """Tests for SulfurasStrategy — legendary item, never changes."""

    def setUp(self):
        self.strategy = SulfurasStrategy()

    def test_quality_never_changes(self):
        item = Item("Sulfuras, Hand of Ragnaros", sell_in=0, quality=80)
        self.strategy.update(item, days=1)
        self.assertEqual(80, item.quality)

    def test_sell_in_never_changes(self):
        item = Item("Sulfuras, Hand of Ragnaros", sell_in=0, quality=80)
        self.strategy.update(item, days=1)
        self.assertEqual(0, item.sell_in)

    def test_unchanged_after_many_days(self):
        item = Item("Sulfuras, Hand of Ragnaros", sell_in=0, quality=80)
        self.strategy.update(item, days=100)
        self.assertEqual(80, item.quality)
        self.assertEqual(0, item.sell_in)
```

- [ ] **Step 2: Run tests to confirm they FAIL**

```bash
python3 -m pytest tests/test_gilded_rose.py::TestSulfurasStrategy -v
```

Expected: `ImportError`.

- [ ] **Step 3: Implement SulfurasStrategy**

Add after `BackstagePassStrategy`:

```python
class SulfurasStrategy:
    """
    No-op strategy for Sulfuras, Hand of Ragnaros.

    Legendary items never degrade and never need to be sold.
    Neither quality nor sell_in is ever mutated.
    """

    def update(self, item: "Item", days: int) -> None:
        pass  # legendary items are immutable
```

- [ ] **Step 4: Run tests to confirm they PASS**

```bash
python3 -m pytest tests/test_gilded_rose.py::TestSulfurasStrategy -v
```

Expected: all 3 tests `PASSED`.

- [ ] **Step 5: Run full suite**

```bash
python3 -m pytest tests/ -v
```

- [ ] **Step 6: Commit**

```bash
git add gilded_rose.py tests/test_gilded_rose.py
git commit -m "feat: add SulfurasStrategy"
```

---

### Task 6: ConjuredStrategy — TDD

**Files:**
- Modify: `tests/test_gilded_rose.py` — add `TestConjuredStrategy`
- Modify: `gilded_rose.py` — add `ConjuredStrategy`

Conjured items degrade at twice the normal rate: `-2/day`, `-4/day` after sell date. Quality floors at `0`.

- [ ] **Step 1: Write failing tests**

Update import:

```python
from gilded_rose import (
    Item, GildedRose, NormalStrategy, AgedBrieStrategy,
    BackstagePassStrategy, SulfurasStrategy, ConjuredStrategy,
)
```

Add:

```python
class TestConjuredStrategy(unittest.TestCase):
    """Tests for ConjuredStrategy — degrades twice as fast as normal."""

    def setUp(self):
        self.strategy = ConjuredStrategy()

    def test_quality_decrements_by_2_each_day(self):
        item = Item("Conjured Mana Cake", sell_in=10, quality=20)
        self.strategy.update(item, days=1)
        self.assertEqual(18, item.quality)
        self.assertEqual(9, item.sell_in)

    def test_quality_floors_at_zero(self):
        item = Item("Conjured Mana Cake", sell_in=5, quality=1)
        self.strategy.update(item, days=1)
        self.assertEqual(0, item.quality)

    def test_quality_degrades_by_4_after_sell_date(self):
        # Past sell date: -4 per day
        item = Item("Conjured Mana Cake", sell_in=0, quality=20)
        self.strategy.update(item, days=1)
        self.assertEqual(16, item.quality)
        self.assertEqual(-1, item.sell_in)

    def test_multi_day_crosses_sell_date(self):
        # sell_in=1, quality=10, days=3
        # Day1: q=8 si=0 | Day2: q=4 si=-1 | Day3: q=0 si=-2
        item = Item("Conjured Mana Cake", sell_in=1, quality=10)
        self.strategy.update(item, days=3)
        self.assertEqual(0, item.quality)
        self.assertEqual(-2, item.sell_in)

    def test_quality_floors_at_zero_post_sell_date(self):
        item = Item("Conjured Mana Cake", sell_in=0, quality=2)
        self.strategy.update(item, days=1)
        self.assertEqual(0, item.quality)
```

- [ ] **Step 2: Run tests to confirm they FAIL**

```bash
python3 -m pytest tests/test_gilded_rose.py::TestConjuredStrategy -v
```

Expected: `ImportError`.

- [ ] **Step 3: Implement ConjuredStrategy**

Add after `SulfurasStrategy`:

```python
class ConjuredStrategy:
    """
    Double-degradation strategy for Conjured items.

    Quality decreases by 2 per day; by 4 per day once the sell date
    has passed. Quality never falls below 0.
    """

    def update(self, item: "Item", days: int) -> None:
        for _ in range(days):
            item.quality = max(0, item.quality - 2)
            item.sell_in -= 1
            if item.sell_in < 0:
                item.quality = max(0, item.quality - 2)
```

- [ ] **Step 4: Run tests to confirm they PASS**

```bash
python3 -m pytest tests/test_gilded_rose.py::TestConjuredStrategy -v
```

Expected: all 5 tests `PASSED`.

- [ ] **Step 5: Run full suite**

```bash
python3 -m pytest tests/ -v
```

- [ ] **Step 6: Commit**

```bash
git add gilded_rose.py tests/test_gilded_rose.py
git commit -m "feat: add ConjuredStrategy"
```

---

### Task 7: Wire GildedRose to use strategies — TDD

**Files:**
- Modify: `gilded_rose.py` — add `ItemRecord`, `_STRATEGY_MAP`, refactor `GildedRose`
- Modify: `tests/test_gilded_rose.py` — add `TestGildedRose` integration tests

This is the wiring step: `GildedRose` replaces its monolithic `update_quality` body with strategy dispatch via `ItemRecord`.

- [ ] **Step 1: Write failing integration tests**

Update import to add `ItemRecord`:

```python
from gilded_rose import (
    Item, GildedRose, NormalStrategy, AgedBrieStrategy,
    BackstagePassStrategy, SulfurasStrategy, ConjuredStrategy,
    ItemRecord,
)
```

Add:

```python
class TestGildedRose(unittest.TestCase):
    """
    Integration tests for GildedRose.

    Verifies strategy dispatch by name and that the days parameter
    is forwarded correctly.
    """

    def test_dispatches_normal_strategy_for_unknown_item(self):
        items = [Item("unknown widget", sell_in=5, quality=10)]
        GildedRose(items).update_quality()
        self.assertEqual(9, items[0].quality)
        self.assertEqual(4, items[0].sell_in)

    def test_dispatches_aged_brie_strategy(self):
        items = [Item("Aged Brie", sell_in=5, quality=20)]
        GildedRose(items).update_quality()
        self.assertEqual(21, items[0].quality)

    def test_dispatches_backstage_pass_strategy(self):
        items = [Item("Backstage passes to a TAFKAL80ETC concert", sell_in=5, quality=20)]
        GildedRose(items).update_quality()
        self.assertEqual(23, items[0].quality)

    def test_dispatches_sulfuras_strategy(self):
        items = [Item("Sulfuras, Hand of Ragnaros", sell_in=0, quality=80)]
        GildedRose(items).update_quality()
        self.assertEqual(80, items[0].quality)
        self.assertEqual(0, items[0].sell_in)

    def test_dispatches_conjured_strategy(self):
        items = [Item("Conjured Mana Cake", sell_in=5, quality=20)]
        GildedRose(items).update_quality()
        self.assertEqual(18, items[0].quality)

    def test_multi_day_update_via_days_param(self):
        items = [Item("unknown widget", sell_in=3, quality=10)]
        GildedRose(items).update_quality(days=3)
        # Day1: q=9 si=2 | Day2: q=8 si=1 | Day3: q=7 si=0
        self.assertEqual(7, items[0].quality)
        self.assertEqual(0, items[0].sell_in)

    def test_multiple_items_updated_independently(self):
        items = [
            Item("Aged Brie", sell_in=5, quality=20),
            Item("unknown widget", sell_in=5, quality=20),
        ]
        GildedRose(items).update_quality()
        self.assertEqual(21, items[0].quality)  # Aged Brie goes up
        self.assertEqual(19, items[1].quality)  # normal goes down

    def test_item_record_allows_direct_strategy_injection(self):
        # Verify ItemRecord can be used to inject a strategy directly in tests
        item = Item("anything", sell_in=5, quality=20)
        record = ItemRecord(item=item, strategy=ConjuredStrategy())
        record.strategy.update(record.item, days=1)
        self.assertEqual(18, item.quality)
```

- [ ] **Step 2: Run tests to confirm they FAIL**

```bash
python3 -m pytest tests/test_gilded_rose.py::TestGildedRose -v
```

Expected: `ImportError` for `ItemRecord`; or `TypeError` on `update_quality(days=3)` since the current implementation doesn't accept `days`.

- [ ] **Step 3: Refactor GildedRose in gilded_rose.py**

Replace the entire `GildedRose` class with the following (keep `Item` class unchanged at the bottom of the file):

```python
_STRATEGY_MAP: dict[str, UpdateStrategy] = {
    "Aged Brie": AgedBrieStrategy(),
    "Backstage passes to a TAFKAL80ETC concert": BackstagePassStrategy(),
    "Sulfuras, Hand of Ragnaros": SulfurasStrategy(),
    "Conjured Mana Cake": ConjuredStrategy(),
}


@dataclass
class ItemRecord:
    """
    Internal pairing of an Item with its UpdateStrategy.

    Built once in GildedRose.__init__ so the name-to-strategy lookup
    happens at construction time, not on every update call.
    Exposed publicly so tests can inject strategies directly without
    going through the name-lookup dict.
    """

    item: Item
    strategy: UpdateStrategy


class GildedRose:
    """
    Inventory manager for the Gilded Rose inn.

    Accepts a list of Item objects and updates their quality and sell_in
    values according to each item's type-specific strategy. The Item
    class and the items list are left untouched per the goblin's terms.

    Usage:
        rose = GildedRose(items)
        rose.update_quality()          # advance one day
        rose.update_quality(days=7)    # advance a full week
    """

    def __init__(self, items: list[Item]) -> None:
        self.items = items
        self._records: list[ItemRecord] = [
            ItemRecord(
                item=i,
                strategy=_STRATEGY_MAP.get(i.name, NormalStrategy()),
            )
            for i in items
        ]

    def update_quality(self, days: int = 1) -> None:
        """Advance quality and sell_in for all items by `days` days."""
        for record in self._records:
            record.strategy.update(record.item, days)
```

- [ ] **Step 4: Run integration tests to confirm they PASS**

```bash
python3 -m pytest tests/test_gilded_rose.py::TestGildedRose -v
```

Expected: all 8 tests `PASSED`.

- [ ] **Step 5: Run full suite to confirm all tests pass**

```bash
python3 -m pytest tests/ -v
```

Expected: all tests across all `TestCase` classes `PASSED`.

- [ ] **Step 6: Commit**

```bash
git add gilded_rose.py tests/test_gilded_rose.py
git commit -m "feat: refactor GildedRose with Strategy pattern and ItemRecord"
```

---

## Verification

Run the full suite from the project root:

```bash
cd /Users/rajvora/Documents/interviews/GildedRose-Refactoring-Kata/python
python3 -m pytest tests/ -v
```

Expected output: all tests in `GildedRoseTest`, `TestNormalStrategy`, `TestAgedBrieStrategy`, `TestBackstagePassStrategy`, `TestSulfurasStrategy`, `TestConjuredStrategy`, and `TestGildedRose` show `PASSED`.

Also verify the texttest fixture still runs:

```bash
python3 texttest_fixture.py 5
```

Expected: 5 days of inventory output with no errors.
