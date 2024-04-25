# Vergulde Roos Requirement Specificaties

Hoi en welkom bij team Vergulde Roos. Zoals je weet, zijn we een klein herberg met een uitstekende locatie in een prominente stad gerund door een vriendelijke herbergier genaamd Allison. We kopen en verkopen ook alleen de beste goederen. Helaas, onze goederen degraderen constant in kwaliteit `Quality` naarmate ze hun uiterste houdbaarheidsdatum naderen.

We hebben een systeem dat onze inventaris voor ons bijwerkt. Het is ontwikkeld door een no-nonsense type genaamd Leeroy, die zich op nieuwe avonturen gestort heeft. Jouw taak is om deze nieuwe functie toe te voegen aan ons systeem zodat we een nieuwe categorie items kunnen gaan verkopen. Eerst een introductie tot ons systeem:

- Alle artikelen `items` hebben een `SellIn` waarde die aangeeft hoeveel dagen we nog hebben om de `items` te verkopen
- Alle `items` hebben een `Quality` (kwaliteit) waarde die aangeeft hoe waardevol het item is
- Aan het einde van elke dag verlagen we beide waarden voor elk item in ons systeem

Vrij eenvoudig, toch? Nou, hier wordt het interessant:

- Zodra de uiterste verkoopdatum is verstreken, degradeert `Quality` twee keer zo snel
- De `Quality` van een item is nooit negatief
- Oude Brie __"Aged Brie"__ neemt eigenlijk toe in `Quality` naarmate het ouder wordt
- De `Quality` van een item is nooit meer dan `50`
- __"Sulfuras"__, als legendarisch item, hoeft nooit te worden verkocht of vermindert niet in `Quality`
- __"Backstage passes"__, zoals aged brie, neemt toe in `Quality` naarmate de `SellIn` waarde nadert;
	- `Quality` neemt met `2` toe wanneer er `10` dagen of minder zijn en met `3` wanneer er `5` dagen of minder zijn, maar
	- `Quality` daalt naar `0` na het concert

We hebben onlangs een leverancier van betoverde items gecontracteerd. Dit vereist een update van ons systeem:

- __"Conjured"__ items degraderen in `Quality` twee keer zo snel als normale items

Voel je vrij om wijzigingen aan te brengen in de `UpdateQuality` methode en voeg nieuwe code toe zolang alles nog steeds correct werkt. Wijzig echter niet de `Item` klasse of `Items` eigenschap aangezien die toebehoren aan de kobold op de hoek die meteen boos wordt en je met één klap uitschakelt omdat hij niet gelooft in gedeeld codebezit (je kunt de `UpdateQuality` methode en `Items` eigenschap wel statisch maken als je wilt, we dekken je wel).

Voor de duidelijkheid, een item kan zijn `Quality` nooit verhogen boven `50`, echter __"Sulfuras"__ is een legendarisch item en als zodanig is zijn `Quality` `80` en verandert nooit.
