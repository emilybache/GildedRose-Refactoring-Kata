require 'rspec'
require File.join(File.dirname(__FILE__), 'gilded_rose')

describe GildedRose do
  it 'does not change the name' do
    items = [Item.new('foo', 0, 0)]
    GildedRose.new(items).update_quality
    expect(items[0].name).to eq 'foo'
  end

  let(:items) do
    [
      Item.new('Aged Brie', 2, 0),
      Item.new('Elixir of the Mongoose', 5, 7),
      Item.new('Sulfuras, Hand of Ragnaros', 0, 80),
      Item.new('Backstage passes to a TAFKAL80ETC concert', 15, 20),
      Item.new('Conjured Mana Cake', 3, 6),
      Item.new('Apple Furit', 10, 20)
    ]
  end

  let(:rose) do
    GildedRose.new(items)
  end

  let('after_1_day_item') do
    {
      'Aged Brie' => [1, 1],
      'Elixir of the Mongoose' => [4, 6],
      'Sulfuras, Hand of Ragnaros' => [0, 80],
      'Backstage passes to a TAFKAL80ETC concert' => [14, 21],
      'Conjured Mana Cake' => [2, 4],
      'Apple Furit' => [9, 19]
    }
  end

  it 'test after 1 day' do
    rose.update_quality

    items.each do |item|
      expect(item.sell_in).to eq after_1_day_item[item.name][0]
      expect(item.quality).to eq after_1_day_item[item.name][1]
    end
  end

  let('after_2_day_item') do
    {
      'Aged Brie' => [0, 2],
      'Elixir of the Mongoose' => [3, 5],
      'Sulfuras, Hand of Ragnaros' => [0, 80],
      'Backstage passes to a TAFKAL80ETC concert' => [13, 22],
      'Conjured Mana Cake' => [1, 2],
      'Apple Furit' => [8, 18]
    }
  end

  it 'test after 2 day' do
    rose.update_quality

    items.each do |item|
      expect(item.sell_in).to eq after_1_day_item[item.name][0]
      expect(item.quality).to eq after_1_day_item[item.name][1]
    end
  end

  let('after_3_day_item') do
    {
      'Aged Brie' => [-1, 4],
      'Elixir of the Mongoose' => [2, 4],
      'Sulfuras, Hand of Ragnaros' => [0, 80],
      'Backstage passes to a TAFKAL80ETC concert' => [12, 23],
      'Conjured Mana Cake' => [0, 0],
      'Apple Furit' => [7, 17]
    }
  end

  it 'test after 3 day' do
    rose.update_quality

    items.each do |item|
      expect(item.sell_in).to eq after_1_day_item[item.name][0]
      expect(item.quality).to eq after_1_day_item[item.name][1]
    end
  end
end
