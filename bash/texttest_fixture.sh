#!/bin/bash

GILDED_ROSE_SCRIPT="./gilded_rose.sh"

# Define initial items as a multi-line string
initial_items="+5 Dexterity Vest|10|20
Aged Brie|2|0
Elixir of the Mongoose|5|7
Sulfuras, Hand of Ragnaros|0|80
Sulfuras, Hand of Ragnaros|-1|80
Backstage passes to a TAFKAL80ETC concert|15|20
Backstage passes to a TAFKAL80ETC concert|10|49
Backstage passes to a TAFKAL80ETC concert|5|49
Conjured Mana Cake|3|6"

simulate_days() {
    local days=$1
    local items="$2"

    for ((day = 0; day <= days; day++)); do
        echo "-------- day $day --------"
        echo "name, sellIn, quality"

        echo "$items" | sed 's/|/, /g'

        # Update the items for the next day
        items=$(echo "$items" | bash "$GILDED_ROSE_SCRIPT")

        echo ""
    done
}

echo OMGHAI!

DAYS_TO_SIMULATE=${1:-2}

simulate_days $DAYS_TO_SIMULATE "$initial_items"
