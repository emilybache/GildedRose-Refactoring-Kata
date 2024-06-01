const std = @import("std");

pub const Item = struct {
    name: []const u8,
    sell_in: i32,
    quality: i32,

    pub fn init(name: []const u8, sell_in: i32, quality: i32) Item {
        return Item{
            .name = name,
            .sell_in = sell_in,
            .quality = quality,
        };
    }
};

pub const GildedRose = struct {
    items: []Item,

    pub fn init(items: []Item) GildedRose {
        return GildedRose{ .items = items };
    }

    pub fn updateQuality(self: *GildedRose) []Item {
        for (0..self.items.len) |i| {
            if (!std.mem.eql(u8, self.items[i].name, "Aged Brie") and !std.mem.eql(u8, self.items[i].name, "Backstage passes to a TAFKAL80ETC concert")) {
                if (self.items[i].quality > 0) {
                    if (!std.mem.eql(u8, self.items[i].name, "Sulfuras, Hand of Ragnaros")) {
                        self.items[i].quality = self.items[i].quality - 1;
                    }
                }
            } else {
                if (self.items[i].quality < 50) {
                    self.items[i].quality = self.items[i].quality + 1;
                    if (std.mem.eql(u8, self.items[i].name, "Backstage passes to a TAFKAL80ETC concert")) {
                        if (self.items[i].sell_in < 11) {
                            if (self.items[i].quality < 50) {
                                self.items[i].quality = self.items[i].quality + 1;
                            }
                        }
                        if (self.items[i].sell_in < 6) {
                            if (self.items[i].quality < 50) {
                                self.items[i].quality = self.items[i].quality + 1;
                            }
                        }
                    }
                }
            }
            if (!std.mem.eql(u8, self.items[i].name, "Sulfuras, Hand of Ragnaros")) {
                self.items[i].sell_in = self.items[i].sell_in - 1;
            }
            if (self.items[i].sell_in < 0) {
                if (!std.mem.eql(u8, self.items[i].name, "Aged Brie")) {
                    if (!std.mem.eql(u8, self.items[i].name, "Backstage passes to a TAFKAL80ETC concert")) {
                        if (self.items[i].quality > 0) {
                            if (!std.mem.eql(u8, self.items[i].name, "Sulfuras, Hand of Ragnaros")) {
                                self.items[i].quality = self.items[i].quality - 1;
                            }
                        }
                    } else {
                        self.items[i].quality = self.items[i].quality - self.items[i].quality;
                    }
                } else {
                    if (self.items[i].quality < 50) {
                        self.items[i].quality = self.items[i].quality + 1;
                    }
                }
            }
        }
        return self.items;
    }
};

test "updateQuality" {
    var items = [_]Item{Item.init("foo", 0, 0)};
    var app = GildedRose.init(&items);
    _ = app.updateQuality();
    try std.testing.expectEqualStrings("fixme", items[0].name);
}
