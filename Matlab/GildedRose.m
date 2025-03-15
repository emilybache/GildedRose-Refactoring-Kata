classdef GildedRose

    properties
        items
    end

    methods
        function obj = GildedRose(items)
            obj.items = items;
        end

        function update_quality(obj)
            for i = 1:length(obj.items)
                item = obj.items(i);
                if item.name ~= "Aged Brie" && item.name ~= "Backstage passes to a TAFKAL80ETC concert"
                    if item.quality > 0
                        if item.name ~= "Sulfuras, Hand of Ragnaros"
                            item.quality = item.quality - 1;
                        end
                    end
                else
                    if item.quality < 50
                        item.quality = item.quality + 1;
                        if item.name == "Backstage passes to a TAFKAL80ETC concert"
                            if item.sell_in < 11
                                if item.quality < 50
                                    item.quality = item.quality + 1;
                                end
                            end
                            if item.sell_in < 6
                                if item.quality < 50
                                    item.quality = item.quality + 1;
                                end
                            end
                        end
                    end
                end
                if item.name ~= "Sulfuras, Hand of Ragnaros"
                    item.sell_in = item.sell_in - 1;
                end
                if item.sell_in < 0
                    if item.name ~= "Aged Brie"
                        if item.name ~= "Backstage passes to a TAFKAL80ETC concert"
                            if item.quality > 0
                                if item.name ~= "Sulfuras, Hand of Ragnaros"
                                    item.quality = item.quality - 1;
                                end
                            end
                        else
                            item.quality = item.quality - item.quality;
                        end
                    else
                        if item.quality < 50
                            item.quality = item.quality + 1;
                        end
                    end
                end
            end
        end
    end
end



