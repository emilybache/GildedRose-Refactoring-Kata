class GildedRose

  def initialize(items)
    @items = items
  end

  def update_quality()
    @items.each do |item|
      if item.name != "Aged Brie" and item.name != "Backstage passes to a TAFKAL80ETC concert"
        if item.quality > 0
          if item.name != "Sulfuras, Hand of Ragnaros"
            item.quality = item.quality - 1
          end
        end
      else
        if item.quality < 50
          item.quality = item.quality + 1
          if item.name == "Backstage passes to a TAFKAL80ETC concert"
            if item.sell_in < 11
              if item.quality < 50
                item.quality = item.quality + 1
              end
            end
            if item.sell_in < 6
              if item.quality < 50
                item.quality = item.quality + 1
              end
            end
          end
        end
      end
      if item.name != "Sulfuras, Hand of Ragnaros"
        item.sell_in = item.sell_in - 1
      end
      if item.sell_in < 0
        if item.name != "Aged Brie"
          if item.name != "Backstage passes to a TAFKAL80ETC concert"
            if item.quality > 0
              if item.name != "Sulfuras, Hand of Ragnaros"
                item.quality = item.quality - 1
              end
            end
          else
            item.quality = item.quality - item.quality
          end
        else
          if item.quality < 50
            item.quality = item.quality + 1
          end
        end
      end
      
      # itemのnameがfooだった場合に、fixmeに更新する
      item.update_name_from_foo_to_fixme
    end
  end
end

# 必ず自身のインスタンス変数を操作するよう実装する
# いいクラスの構成要素
#   インスタンス変数
#   完全性を保証するようにインスタンス変数を操作するメソッド

# コンストラクタで初期化する
# initializeメソッドを実装する
class Item
  attr_accessor :sell_in, :quality
  attr_reader :name

  def initialize(name, sell_in, quality)
    @name = name
    @sell_in = sell_in
    @quality = quality
  end

  def to_s()
    "#{@name}, #{@sell_in}, #{@quality}"
  end

  def update_name_from_foo_to_fixme
    return if can_change_name_fixme?
    @name = "fixme"
  end

  private
  
  def can_change_name_fixme?
    @name != "foo"
  end
end
