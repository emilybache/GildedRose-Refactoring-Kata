#!/bin/bash

GILDED_ROSE_SCRIPT="./gilded_rose.sh"

create_initial_items() {
    local temp_file=$(mktemp)

    cat <<EOF >"$temp_file"
+5 Dexterity Vest|10|20
Aged Brie|2|0
Elixir of the Mongoose|5|7
Sulfuras, Hand of Ragnaros|0|80
Sulfuras, Hand of Ragnaros|-1|80
Backstage passes to a TAFKAL80ETC concert|15|20
Backstage passes to a TAFKAL80ETC concert|10|49
Backstage passes to a TAFKAL80ETC concert|5|49
Conjured Mana Cake|3|6
EOF

    echo "$temp_file"
}

simulate_days() {
    local days=$1
    local items_file=$2

    for ((day = 0; day <= days; day++)); do
        echo "-------- day $day --------"
        echo "name, sellIn, quality"

        cat "$items_file" | sed 's/|/, /g'

        local temp_output=$(mktemp)
        cat "$items_file" | bash "$GILDED_ROSE_SCRIPT" >"$temp_output"
        mv "$temp_output" "$items_file"

        echo ""
    done
}

echo OMGHAI!

ITEMS_FILE=$(create_initial_items)

DAYS_TO_SIMULATE=${1:-2}

simulate_days $DAYS_TO_SIMULATE $ITEMS_FILE

rm "$ITEMS_FILE"
