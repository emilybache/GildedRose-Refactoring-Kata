A pure "function", so this works:

```shell
$ echo -e 'Aged Brie,3,5\nOther Item,4,5' |\
    ./gilded_rose.sh |\
    ./gilded_rose.sh |\
    ./gilded_rose.sh
Aged Brie,0,8
Other Item,1,2
```