# Urrezko Arrosaren zehaztapenak (Gilded Rose)

Ongi etorri **Gilded Rose**-eko taldera.
Jakingo duzunez, hiri ospetsu batean estrategikoki kokatutako ostatu txiki bat gara, Allison atseginak zuzendua. Kalitate oneneko salgaiak ere erosten eta saltzen ditugu. Zoritxarrez, gure salgaiak kalitatez jaisten doaz salmenta-data hurbildu ahala.

Inbentarioa automatikoki eguneratzen duen sistema bat dugu instalaturik. Gaur egun abentura berrietan dabilen Leeroy izeneko mutiko zentzugabe batek garatu zuen sistema hau. Zure zeregina sistemari ezaugarri berri bat gehitzea da, artikulu kategoria berri bat saltzen has gaitezen.

## Hasierako deskribapena

Baina lehenik, sistema azalduko dugu:

* Artikulu guztiek (`Item`) `sellIn` izeneko propietate bat dute berau saltzeko ditugun egunen kopurua adierazten duena.
* Artikulu guztiek `quality` izeneko propietate bat dute artikulu hori zein baliotsua den adierazten duena.
* Egunaren amaieran, gure sistemak artikulu bakoitzerako bi balio horiek gutxitzen ditu `updateQuality` metodoaren bitartez.

Nahiko sinplea, ezta? Beno, orain da interesgarri jartzen denean:

* Behin gomendatutako salmenta-data igarota, kalitatea (`quality`) bi aldiz azkarrago degradatzen da.
* Artikulu baten kalitatea (`quality`) ez da inoiz negatiboa.
* “Aged brie”-ren kalitatea (`quality`) hobetu egiten da zahartu ahala unitate `1` gehituz egun bakoitzeko.
* Artikulu baten kalitatea (`quality`) ezin da inoiz `50` baino handiagoa izan.
* "Sulfuras" artikuluak, artikulu legendarioa izanik, ez du salmenta-data aldatzen eta bere kalitatea ez da degradatzen.
* "Backstage Passes"-ak, "Aged brie”-a bezala, kalitatean (`quality`) hobetzen doaz salmenta-data (`sellIn`) hurbildu ahala:
  * Salmenta-datarako (`sellIn`) `10` egun edo gutxiago falta badira, kalitatea (`quality`) `2` unitatetan haundituko da.
  * Salmenta-datarako (`sellIn`) `5` egun edo gutxiago falta badira, kalitatea (`quality`) `3` unitatetan haundituko da.
  * Salmenta-data (`sellIn`) pasa ondoren, kalitatea (`quality`) `0`ra pasako da.

Duela gutxi, konjuratutako artikulu hornitzaile bat kontratatu genuen. Horretarako, sistema eguneratu behar da:

* Konjuratutako (`conjured`) artikuluek normalek baino `2` aldiz azkarrago degradatzen dute kalitatea (`quality`).

Lasai alda dezakez `updateQuality` metodoa beharrezkoa ikusten duzu kodea gehitzeko, beti ere, denak behar bezela funtzionatzen jarraitzen duen bitartean. Baina ezin duzu ordea `Item` objektua ezta bere propietaterik aldatu, txokoan dagoen goblinarenak bait dira eta haserre-krisi batean erasotu egin zaitzake ez bait du kode jabetza partekatuan sinisten.

## Azken oharrak

Argitzearren, artikulu batek ezin du inoiz `50` baino kalitate (`quality`) handiagoa izan, "Sulfuras" ordea, artikulu legendario bat izanik, `80`ko kalitate aldaezina dauka.
