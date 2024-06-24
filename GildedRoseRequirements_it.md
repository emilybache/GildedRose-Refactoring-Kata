# Requisiti della rosa dorata (Gilded Rose)


Ciao, benvenuto nel team **Rosa dorata**.  
Come sapete, siamo una piccola locanda con una posizione privilegiata in una importante città  
gestita da un amichevole locandiere di nome Allison.
Compriamo e vendiamo solo i prodotti migliori.

Sfortunatamente, la qualità dei nostri prodotti diminuisce costantemente man mano che si avvicinano alla data di scadenza.  
Disponiamo di un sistema che aggiorna il nostro inventario in automatico.  
Il sistema è stato sviluppato da un tipo pratico chiamato Leeroy, che è passato a nuove avventure.  

Il tuo compito è aggiungere una nuova funzionalità al nostro sistema in modo che possiamo iniziare a vendere una nuova categoria di articoli. 

## Decrizione del sistema:

- Tutti i prodotti (`Item`) hanno una proprietà `sellIn` che indica quanti giorni mancano alla data di scadenza.
- Tutti i prodotti (`Item`) hanno una proprietà `quality` che denota il valore dell'articolo.
- Alla fine di ogni giornata il nostro sistema decrementa entrambe le proprietà per ogni prodotto tramite il metodo `updateQuality`

Abbastanza semplice, vero? Bene, è da qui che la cosa si fa interessante:

- Una volta passata la data di scadenza, la proprietà `quality` diminuisce due volte più velocemente
- La proprietà `quality` di un prodotto non può essere mai negativa
- Il prodotto "Brie invecchiato" (`Aged brie`) aumenta di uno la sua `quality` man mano che invecchia
- La `quality` di un prodotto non può mai essere superiore a 50
- Il prodotto "Sulfuras" (`Sulfuras`), essendo un oggetto leggendario, non modifica mai ne la proprietà `sellIn` ne degrada la proprietà `quality`
- I prodotto "Backstage pass" (`Backstage pass`), come il brie invecchiato (`Aged brie`), aumentano `quality` man mano che il loro valore di `sellIn` si avvicina a 0
    - La proprietà `quality` aumenta di 2 quando mancano 10 giorni o meno e di 3 quando ci sono 5 giorni o meno ma,
    - La proprietà `quality` scende a 0 quando il valore di `sellIn` scende a 0.

## La nuova richiesta:

Recentemente è stato firmato un contratto con un fornitore di oggetti "oggetti magici" (`conjurados`)
Ciò richiede un aggiornamento del nostro sistema:

- Gli "oggetti magici" (`conjurados`) diminuiscono di `quality` due volte più velocemente rispetto ai prodotti normali.

Sentiti libero di apportare qualsiasi modifica al metodo "updateQuality" ed aggiungere codice se necessario, purché tutto continui a funzionare correttamente.
Tuttavia, **non alterare l'oggetto `Item` o le sue proprietà** poiché appartengono al goblin nell'angolo, che in un impeto di rabbia ti colpirà perché non crede nella cultura della condivisione del codice.

## Note finali:

- Un prodotto non può mai avere un aumento di qualità `quality` superiore a 50, tuttavia 
- il prodotto "Sulfuras" (`Sulfuras`) è un oggetto leggendario e come tale la sua Qualità `quality` è 80 e non si altera mai.
