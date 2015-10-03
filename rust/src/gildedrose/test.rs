use super::{Item, GildedRose};

#[test]
pub fn foo() {
    let items = vec![Item::new(String::from("foo"), 0, 0)];
    let mut rose = GildedRose::new(items);
    rose.update_quality();

    assert_eq!("fixme", rose.items[0].name);
}
