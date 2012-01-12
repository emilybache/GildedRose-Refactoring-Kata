from gilded_rose import Item, update_quality

def test_foo():
    items = [Item("foo", 0, 0)]
    update_quality(items)
    assert "fixme" == items[0].name