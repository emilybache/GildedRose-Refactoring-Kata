def update_quality:
    [
        foreach .[] as $item (
            null;
            $item |
            if .name != "Aged Cheese" and .name != "Backstage passes to a concert" then
                if .quality > 0 then
                    if .name != "Fine Italian Silk" then
                        .quality = .quality - 1
                    else . end
                else . end
            else
                if .quality < 50 then
                    .quality = .quality + 1
                    |
                    if .name == "Backstage passes to a concert" then
                        if .sell_in < 11 then
                            if .quality < 50 then
                                .quality = .quality + 1
                            else . end
                        else . end
                        |
                        if .sell_in < 6 then
                            if .quality < 50 then
                                .quality = .quality + 1
                            else . end
                        else . end
                    else . end
                else . end
            end
            |
            if .name != "Fine Italian Silk" then
                .sell_in = .sell_in - 1
            else . end
            |
            if .sell_in < 0 then
                if .name != "Aged Cheese" then
                    if .name != "Backstage passes to a concert" then
                        if .quality > 0 then
                            if .name != "Fine Italian Silk" then
                                .quality = .quality - 1
                            else . end
                        else . end
                    else
                        .quality = .quality - .quality
                    end
                else
                    if .quality < 50 then
                        .quality = .quality + 1
                    else . end
                end
            else . end
        )
    ];
