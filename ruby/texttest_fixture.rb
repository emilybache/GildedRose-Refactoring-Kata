#!/usr/bin/ruby -w

require File.join(File.dirname(__FILE__), 'gilded_rose')

puts "OMGHAI!"
items = [
  Item.new("+5 Dexterity Vest", 10, 20),
  Item.new("Aged Brie", 2, 0),
  Item.new("Elixir of the Mongoose", 5, 7),
  Item.new("Sulfuras, Hand of Ragnaros", 0, 80),
  Item.new("Sulfuras, Hand of Ragnaros", -1, 80),
  Item.new("Backstage passes to a TAFKAL80ETC concert", 15, 20),
  Item.new("Backstage passes to a TAFKAL80ETC concert", 10, 49),
  Item.new("Backstage passes to a TAFKAL80ETC concert", 5, 49),
  # This Conjured item does not work properly yet
  Item.new("Conjured Mana Cake", 3, 6), # <-- :O
]

days = 2
if ARGV.size > 0
  days = ARGV[0].to_i + 1
end

gilded_rose = GildedRose.new items
(0...days).each do |day|
  puts "-------- day #{day} --------"
  puts "name, sellIn, quality"
  items.each do |item|
    puts item
  end
  puts ""
  gilded_rose.update_quality
end
