#!/bin/bash

update_quality() {
    local IFS='|'

    while read -r name sell_in_str quality_str; do
        declare -i sell_in="$sell_in_str"
        declare -i quality="$quality_str"

        if [[ $name != "Aged Brie" && $name != "Backstage passes to a TAFKAL80ETC concert" ]]; then
            if ((quality > 0)); then
                if [[ $name != "Sulfuras, Hand of Ragnaros" ]]; then
                    ((quality--))
                fi
            fi
        else
            if ((quality < 50)); then
                ((quality++))

                if [[ $name == "Backstage passes to a TAFKAL80ETC concert" ]]; then
                    if ((sell_in < 11)) && ((quality < 50)); then
                        ((quality++))
                    fi
                    if ((sell_in < 6)) && ((quality < 50)); then
                        ((quality++))
                    fi
                fi
            fi
        fi

        if [[ $name != "Sulfuras, Hand of Ragnaros" ]]; then
            ((sell_in--))

            if ((sell_in < 0)); then
                if [[ $name != "Aged Brie" ]]; then
                    if [[ $name != "Backstage passes to a TAFKAL80ETC concert" ]]; then
                        if ((quality > 0)); then
                            if [[ $name != "Sulfuras, Hand of Ragnaros" ]]; then
                                ((quality--))
                            fi
                        fi
                    else
                        quality=0
                    fi
                else
                    if ((quality < 50)); then
                        ((quality++))
                    fi
                fi
            fi
        fi

        echo "$name|$sell_in|$quality"
    done
}

update_quality
