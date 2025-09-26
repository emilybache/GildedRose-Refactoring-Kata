# Especificacións da Rosa Dourada (Gilded Rose)

Benvido ao equipo **Gilded Rose**.
Como saberás, somos unha pequena pousada situada estratexicamente nunha cidade prestixiosa, atendida pola amable Allison. Tamén compramos e vendemos mercadoría de alta calidade. Por desgraza, a nosa mercadoría vai perdendo calidade (`Quality`) a medida que se achega a data de venda.

Temos un sistema instalado que actualiza automaticamente o noso inventario. Este sistema foi desenvolvido por un tipo serio e práctico chamado Leeroy, que agora está noutras aventuras.

A túa tarefa é engadir unha nova funcionalidade ao sistema para que poidamos comezar a vender unha nova categoría de artigos. Pero primeiro, imos describir como funciona o sistema:

## Descrición do sistema

- Todos os artigos (`item`) teñen unha propiedade `SellIn` que indica o número de días que temos para vendelo
- Todos os artigos (`item`) teñen unha propiedade `Quality` que indica o valioso que é o artigo
- Ao final de cada día, o noso sistema actualiza ambos valores para cada artigo mediante o método `updateQuality`

Bastante sinxelo, non? Pois agora é cando se pon interesante:

- Unha vez que pasa a data recomendada de venda (`SellIn`), a calidade (`Quality`) degrádase o dobre de rápido
- A `Quality` dun artigo nunca é negativa
- O "Queixo Brie curado" (`Aged brie`) incrementa a súa calidade (`Quality`) a medida que madura cos días
  - A súa calidade (`Quality`) aumenta en `1` unidade cada día
  - Unha vez expirada a data de venda (`SellIn`), a súa calidade (`Quality`) aumenta o dobre cada día
- A calidade dun artigo (`Quality`) non pode superar `50`
- O artigo `Sulfuras` é un artigo lendario, non precisamos vendelo en ningunha data (`SellIn`) e tampouco se degrada en (`Quality`)
- O artigo "Entrada ao Backstage" (`Backstage passes`), incrementa a súa calidade (`Quality`) a medida que se achega a data do concerto (`SellIn`)
  - Se faltan `10` días ou menos para o concerto, a calidade (`Quality`) increméntase en `2` unidades
  - Se faltan `5` días ou menos, a calidade (`Quality`) increméntase en `3` unidades
  - Unha vez pasada a data do concerto (`SellIn`), a entrada perde todo o seu valor e a súa calidade (`Quality`) cae a `0`

## A túa tarefa

Hai pouco contratamos a un provedor de artigos _conxurados máxicamente_.
Isto require unha actualización do sistema:

- Os artigos conxurados (`Conjured`) degradan a súa calidade (`Quality`) o dobre de rápido que os artigos normais

Séntete libre de modificar o método `updateQuality` e engadir o código que sexa necesario, sempre e cando todo siga funcionando correctamente. Porén, **non debes modificar a clase `Item` nin as súas propiedades**, xa que pertence a un duende que, nun ataque de ira, podería liquidarte dun golpe porque non cre na cultura de código compartido.

## Notas finais

Para aclarar: un artigo nunca pode ter unha calidade (`Quality`) superior a `50`, aínda que o obxecto `Sulfuras`, ao ser un artigo lendario, teñen unha calidade inmutable de `80`.
