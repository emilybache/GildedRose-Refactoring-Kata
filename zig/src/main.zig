const std = @import("std");
const gilded_rose = @import("gilded_rose.zig");
const Item = gilded_rose.Item;
const GildedRose = gilded_rose.GildedRose;

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const args = try std.process.argsAlloc(allocator);
    var days: u8 = 2;
    if (args.len >= 2) {
        days = try std.fmt.parseInt(u8, args[1], 10);
    }

    var items = [_]Item{
        Item.init("+5 Dexterity Vest", 10, 20),
        Item.init("Aged Brie", 2, 0),
        Item.init("Elixir of the Mongoose", 5, 7),
        Item.init("Sulfuras, Hand of Ragnaros", 0, 80),
        Item.init("Sulfuras, Hand of Ragnaros", -1, 80),
        Item.init("Backstage passes to a TAFKAL80ETC concert", 15, 20),
        Item.init("Backstage passes to a TAFKAL80ETC concert", 10, 49),
        Item.init("Backstage passes to a TAFKAL80ETC concert", 5, 49),
        // this Conjured item doesn't yet work properly
        Item.init("Conjured Mana Cake", 3, 6),
    };
    var app = GildedRose.init(&items);

    const stdout = std.io.getStdOut().writer();
    try stdout.print("OMGHAI!\n", .{});

    for (0..days + 1) |day| {
        try stdout.print("-------- day {d} --------\n", .{day});
        try stdout.print("name, sellIn, quality\n", .{});
        for (items) |item| {
            try stdout.print("{s}, {d}, {d}\n", .{ item.name, item.sell_in, item.quality });
        }
        try stdout.print("\n", .{});
        _ = app.updateQuality();
    }
}
