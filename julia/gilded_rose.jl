import Base

mutable struct Item{T<:Integer}
    name::String
    sellin::T
    quality::T
end

Base.show(io::IO, x::Item) = print(io, "$(x.name), $(x.sellin), $(x.quality)")

struct GildedRose
    items
end

function update_quality!(gr::GildedRose)
    for item in gr.items
        if item.name != "Aged Brie" && item.name != "Backstage passes to a TAFKAL80ETC concert"
            if item.quality > 0
                if item.name != "Sulfuras, Hand of Ragnaros"
                    item.quality = item.quality - 1
                end
            end
        else
            if item.quality < 50
                item.quality = item.quality + 1
                if item.name == "Backstage passes to a TAFKAL80ETC concert"
                    if item.sellin < 11
                        if item.quality < 50
                            item.quality = item.quality + 1
                        end
                    end
                    if item.sellin < 6
                        if item.quality < 50
                            item.quality = item.quality + 1
                        end
                    end
                end
            end
        end
        if item.name != "Sulfuras, Hand of Ragnaros"
            item.sellin = item.sellin - 1
        end
        if item.sellin < 0
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
    end
    return nothing
end

# Technically, julia espouses a REPL-driven workflow, so the preferred way to run this
# would be from the REPL. However, if you'd like to run this function from the
# commandline, run `$ julia -e 'include("gilded_rose.jl"); main(;days=3)'` or whatever
# number you want for `days`.
function main(; days::Int64=2)
    println("OMGHAI!")
    items = [
        Item("+5 Dexterity Vest", 10, 20),
        Item("Aged Brie", 2, 0),
        Item("Elixir of the Mongoose", 5, 7),
        Item("Sulfuras, Hand of Ragnaros", 0, 80),
        Item("Sulfuras, Hand of Ragnaros", -1, 80),
        Item("Backstage passes to a TAFKAL80ETC concert", 15, 20),
        Item("Backstage passes to a TAFKAL80ETC concert", 10, 49),
        Item("Backstage passes to a TAFKAL80ETC concert", 5, 49),
        Item("Conjured Mana Cake", 3, 6),
    ]
    for day in 1:days
        println("-------- day $day --------")
        println("name, sellin, quality")
        for item in items
            println(item)
        end
        update_quality!(GildedRose(items))
    end
end
