# A list of observed potential bugs to investigate

## Conjured Items

"We have recently signed a supplier of conjured items. This requires an update to our system:
"Conjured" items degrade in Quality twice as fast as normal items"

```
>go run texttest_fixture.go 5
...

-------- day 0 --------
Name, SellIn, Quality
...
&{Conjured Mana Cake 3 6}

...
-------- day 1 --------
&{Conjured Mana Cake 2 5}

...
-------- day 2 --------
&{Conjured Mana Cake 1 4}

...
-------- day 3 --------
&{Conjured Mana Cake 0 3}
...
-------- day 4 --------
&{Conjured Mana Cake -1 1}
...
-------- day 5 --------
&{Conjured Mana Cake -2 0}

```

Appears that Qaulity jumps by 2 on data 4 (i.e. quality is never 2)

## aged brie

"Once the sell by date has passed, Quality degrades twice as fast"
"Aged Brie" actually increases in Quality the older it gets
initial behaviour is that quality increases twice as fast after SellIn
