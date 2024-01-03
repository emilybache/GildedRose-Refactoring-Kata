class GildedRose

  def initialize(items)
    @items = items
  end

  def update_quality()
    # Loop through each item in the list
    @items.each do |item|
      # Check the name of the item to determine the update rules
      case item.name
      when "Aged Brie"
        increase_quality(item) # Increase quality of Aged Brie
      when "Backstage passes to a TAFKAL80ETC concert"
        handle_backstage_pass(item) # Handle Backstage Passes quality
      when "Sulfuras, Hand of Ragnaros"
        # Sulfuras does not change in quality or sell_in, so skip to the next item
        next
      when "Conjured"
        degrade_quality(item, 2) # Degrade quality twice as fast for Conjured items
      else
        degrade_quality(item) # Normal degradation for other items
      end

      decrease_sell_in(item) # Decrease the sell_in for all items

      # Check if the item has expired
      if item.sell_in < 0
        handle_expired_item(item) # Handle item quality after expiration
      end
    end
  end

  private

  def increase_quality(item, rate = 1)
    # Increase the quality of an item if it's less than 50 (maximum quality)
    item.quality += rate if item.quality < 50
  end

  def handle_backstage_pass(item)
    # Increase quality based on sell_in period for Backstage Passes
    if item.sell_in > 10
      increase_quality(item) # More than 10 days left: quality increases by 1
    elsif item.sell_in > 5
      increase_quality(item, 2) # 6-10 days left: quality increases by 2
    elsif item.sell_in >= 0
      increase_quality(item, 3) # 1-5 days left: quality increases by 3
    else
      item.quality = 0 # Quality drops to 0 after the concert
    end
  end

  def degrade_quality(item, rate = 1)
    # Decrease the quality of the item, accounting for Conjured items that degrade twice as fast
    if item.name == "Conjured"
      item.quality -= rate * 2 if item.quality > 0
    else
      item.quality -= rate if item.quality > 0
    end
  end

  def decrease_sell_in(item)
    # Decrease the sell_in value for all items except Sulfuras
    item.sell_in -= 1 unless item.name == "Sulfuras, Hand of Ragnaros"
  end

  def handle_expired_item(item)
    # Handle item quality after expiration based on item name
    case item.name
    when "Aged Brie"
      increase_quality(item) # Quality increases after expiration for Aged Brie
    when "Backstage passes to a TAFKAL80ETC concert"
      item.quality = 0 # Quality drops to 0 after the concert
    when "Conjured"
      degrade_quality(item, 4) # Conjured items degrade twice as fast after expiration
    else
      degrade_quality(item, 2) # Normal quality degradation after expiration for other items
    end
  end
end
