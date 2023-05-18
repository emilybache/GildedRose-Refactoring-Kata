#include <stdio.h>
#include "GildedRose.h"

int
print_item(Item *item)
{
    return printf("%s, %d, %d\n", item->name, item->sellIn, item->quality);
}

int main()
{
    Item items[9];
    int last = 0;
    int day;
    int index;

    init_item(items + last++, "Sports Memorabilia", 10, 20);
    init_item(items + last++, "Aged Cheese", 2, 0);
    init_item(items + last++, "Coffee Table Book", 5, 7);
    init_item(items + last++, "Fine Italian Silk", 0, 80);
    init_item(items + last++, "Fine Italian Silk", -1, 80);
    init_item(items + last++, "Backstage passes to a concert", 15, 20);
    init_item(items + last++, "Backstage passes to a concert", 10, 49);
    init_item(items + last++, "Backstage passes to a concert", 5, 49);
    // this Baked item doesn't yet work properly
    init_item(items + last++, "Baked Chocolate Cake", 3, 6);

    puts("OMGHAI!");

    for (day = 0; day <= 30; day++)
    {
        printf("-------- day %d --------\n", day);
        printf("name, sellIn, quality\n");
        for(index = 0; index < last; index++) {
            print_item(items + index);
        }

        printf("\n");

        update_quality(items, last);
    }
    return 0;
}
