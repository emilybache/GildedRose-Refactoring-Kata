# Especificações de Requisitos de Gilded Rose

Bem-vindo ao time Gilded Rose. Como você deve saber, nós somos uma pequena pousada estrategicamente localizada em uma prestigiosa cidade, atendida pelo amigavel atendente Allison. Além de ser uma pousada, nós também compramos e vendemos as mercadorias de melhor qualidade. Infelizmente nossas mercadorias vão perdendo a qualidade conforme chegam próximo sua data de venda.

Nós temos um sistema instalado que atualiza automaticamente os preços do nosso estoque. Esse sistema foi criado por um rapaz sem noção chamado Leeroy, que agora se dedica à novas aventuras. Seu trabalho será adicionar uma nova funcionalidade para o nosso sistema para que possamos vender uma nova categoria de itens. 

## Descrição preliminar

Vamos dar uma breve introdução do nosso sistema:

* Todos os itens (classe `Item`) possuem uma propriedade chamada `SellIn` que informa o número de dias que temos para vende-lo
* Todos os itens possuem uma propriedade chamada `quality` que informa o quão valioso é o item.
* No final do dia, nosso sistema decrementa os valores das propriedades `SellIn` e `quality` de cada um dos itens do estoque através do método `updateQuality`.

Bastante simples, não é? Bem, agora que as coisas ficam interessantes:

* Quando a data de venda do item tiver passado, a qualidade (`quality`) do item diminui duas vezes mais rapido.
* A qualidade (`quality`) do item não pode ser negativa
* O "Queijo Brie envelhecido" (`Aged Brie`), aumenta sua qualidade (`quality`) em `1` unidade a medida que envelhece.
* A qualidade (`quality`) de um item não pode ser maior que 50.
* O item "Sulfuras" (`Sulfuras`), por ser um item lendário, não precisa ter uma data de venda (`SellIn`) e sua qualidade (`quality`) não precisa ser diminuida.
* O item "Entrada para os Bastidores" (`Backstage Passes`), assim como o "Queijo Brie envelhecido", aumenta sua qualidade (`quality`) a medida que o dia da venda (`SellIn`) se aproxima;
  * A qualidade (`quality`) aumenta em `2` unidades quando a data de venda (`SellIn`) é igual ou menor que `10`.
  * A qualidade (`quality`) aumenta em `3` unidades quando a data de venda (`SellIn`) é igual ou menor que `5`.
  * A qualidade (`quality`) do item vai direto à `0` quando a data de venda (`SellIn`) tiver passado.

Nós recentemente assinamos um suprimento de itens Conjurados Magicamente. Isto requer que nós atualizemos nosso sistema:

* Os itens "Conjurados" (`Conjured`) diminuem a qualidade (`quality`) duas vezes mais rápido que os outros itens.

Sinta-se livre para fazer qualquer alteração no método `updateQuality` e adicionar código novo contanto que tudo continue funcionando perfeitamente. Entretanto, não altere o código da classe `Item` ou da propriedade `Items` na classe `GildedRose` pois elas pertencem ao Goblin que irá te matar com um golpe pois ele não acredita na cultura de código compartilhado.

## Notas Finais

Para esclarecer: Um item não pode ter uma qualidade (`quality`) maior que `50`, entretanto as "Sulfuras" por serem um item lendário vão ter uma qualidade imutavel de `80`.
