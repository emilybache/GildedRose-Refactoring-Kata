# Specyfikacja wymagań Pozłacanej Róży (Gilded Rose)


Cześć i witaj na pokładzie zespołu Pozłacanej Róży. Jak zapewne już wiesz, jesteśmy niewielką karczmą, która znajduje się w głównej części wspaniałego miasta i jest prowadzona przez przyjazną oberżystkę o imieniu Allison. Sprzedajemy i kupujemy tylko najlepsze towary. Niestety, przedmioty te tracą na jakości w miarę jak zbliża się ich termin sprzedaży. Korzystamy z systemu, który automatycznie aktualizuje stan naszego inwentarza. System ten został napisany przez rozsądnego typka o imieniu Leeroy, który postanowił poszukać nowych przygód. Twoim zadaniem jest dodanie nowej funkcjonalności do naszego systemu tak, abyśmy mogli rozpocząć sprzedaż nowego rodzaju przedmiotów. Pozwól, że najpierw zrobię ogólne wprowadzenie do systemu:
- Wszystkie przedmioty (`Item`) posiadają właściwość `SellIn`, która oznacza **liczbę dni pozostałych do upłynięcia terminu sprzedaży** przedmiotu
- Wszystkie przedmioty posiadają właściwość `Quality` (**jakość**), która wpływa na wartość przedmiotu
- Na koniec każdego dnia nasz system obniża wartość obu właściwości dla każdego przedmiotu

Dość proste, prawda? No cóż, teraz zrobi się bardziej interesująco:
- Po upływie daty sprzedaży, jakość spada dwukrotnie szybciej
- Jakość przedmiotu nigdy nie jest ujemna
- Jakość "Starego Brie" (`Aged Brie`) rośnie wraz z wiekiem
- Jakość przedmiotu nigdy nie przekracza 50
- Przedmiot legendarny `Sulfuras` nigdy nie musi być sprzedany, ani nie traci na jakości
- "Przepustka za kulisy" (`Backstage passes`), podobnie jak "Stary Brie", zyskują na jakości w miarę zbliżania się terminu sprzedaży; jakość wzrasta o 2, gdy jest 10 dni lub mniej i o 3, gdy jest 5 dni lub mniej, ale spada do 0 po koncercie (gdy `SellIn` < 0)

Niedawno podpisaliśmy z dostawcą kontrakt na wyczarowane przedmioty. Wymaga to wprowadzenia zmiany do naszego systemu:
- "Wyczarowane" (`Conjured`) przedmioty tracą na jakości dwa razy szybciej niż normalne przedmioty

Możesz wprowadzać dowolne zmiany w metodzie `UpdateQuality`, a także dodawać nowy kod, o ile wszystko nadal działa prawidłowo. Jednak nie zmieniaj klasy `Item` ani właściwości `Items`, które zostały napisane przez goblina w rogu, gdyż zaatakuje Cię on i zabije jednym strzałem, ponieważ nie wierzy we współdzielony kod (możesz zmienić metodę `UpdateQuality` oraz właściwość `Items` na statyczne, jeśli chcesz - będziemy Cię kryć!).

Dla jasności: jakość przedmiotu nie może przekroczyć 50, jednak dla przedmiotu legendarnego `Sulfuras` jakość jest stale na poziomie 80 i nigdy nie spada.
