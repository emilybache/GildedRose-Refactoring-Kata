# Spécification de la Rose dorée (Gilded Rose)

Bonjour et bienvenue dans l'équipe de la Rose dorée.

Comme vous le savez, notre petite taverne située à proximité d'une cité importante est dirigée par l'amicale aubergiste Allison.

Nous achetons et vendons uniquement les meilleurs produits.
Malheureusement, la qualité de nos marchandises se dégrade constamment à l'approche de leur date de péremption.

Un système a été mis en place pour mettre à jour notre inventaire.
Il a été développé par Leeroy, une personne pleine de bon sens qui est partie pour de nouvelles aventures.

Votre mission est d'ajouter une nouvelle fonctionnalité à notre système pour que nous puissions commencer à vendre un nouveau type de produits.

Mais d'abord, laissez-moi vous présenter notre système :

- Tous les éléments ont une valeur `sellIn` qui désigne le nombre de jours restant pour vendre l'article.
- Tous les articles ont une valeur `quality` qui dénote combien l'article est précieux.
- À la fin de chaque journée, notre système diminue ces deux valeurs pour chaque produit.

Plutôt simple, non ?

Attendez, ça devient intéressant :

- Une fois que la date de péremption est passée, la qualité se dégrade deux fois plus rapidement.
- La qualité (`quality`) d'un produit ne peut jamais être négative.
- "Aged Brie" augmente sa qualité (`quality`) plus le temps passe.
- La qualité d'un produit n'est jamais de plus de 50.
- "Sulfuras", étant un objet légendaire, n'a pas de date de péremption et ne perd jamais en qualité (`quality`)
- "Backstage passes", comme le "Aged Brie", augmente sa qualité (`quality`) plus le temps passe (`sellIn`) ; La qualité augmente de 2 quand il reste 10 jours ou moins et de 3 quand il reste 5 jours ou moins, mais la qualité tombe à 0 après le concert.

Nous avons récemment signé un partenariat avec un fournisseur de produit invoqué ("Conjured").
Cela nécessite une mise à jour de notre système :

- les éléments "Conjured" voient leur qualité se dégrader de deux fois plus vite que les objets normaux.

Vous pouvez faire les changements que vous voulez à la méthode `updateQuality` et ajouter autant de code que vous voulez, tant que tout fonctionne correctement.
Cependant, nous devons vous prévenir, vous ne devez en aucun cas modifier la classe `Item` ou ses propriétés car cette classe appartient au gobelin à l'étage qui entrerait dans une rage instantanée et vous tuerait sans délai : il ne croit pas au partage du code.
(Vous pouvez ajouter rendre la méthode `updateQuality` statique, ainsi que des propriétés dans la classe `Item` si vous voulez, nous vous couvrirons)

Juste une précision, un produit ne peut jamais voir sa qualité augmenter au-dessus de 50, cependant "Sulfuras" est un objet légendaire et comme tel sa qualité est de 80 et elle ne change jamais.
