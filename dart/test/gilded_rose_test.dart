import 'package:test/test.dart';
import 'package:gilded_rose/gilded_rose.dart';

main() {
  test('foo', () {
    var item = new Item('foo', 0, 0);
    var items = <Item>[item];

    GildedRose app = new GildedRose(items);
    app.updateQuality();
    expect("fixme", app.items[0].name);
  });
}
