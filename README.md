# Gilded Rose Refactoring Kata

## Análisis Inicial de Requisitos: Gilded Rose1. 

### 1. ¿Cuántos tipos de ítems diferentes se mencionan y cuál es su comportamiento?
Se mencionan 5 tipos de artículos con comportamientos específicos:  
- **Items Normales:** La calidad disminuye en 1 cada día. Si la fecha de venta ha pasado (SellIn < 0), disminuye en 2.  
- **Aged Brie (Queso Brie envejecido):** La calidad aumenta en 1 cada día. Si la fecha de venta ha pasado, la calidad aumenta en 2.  
- **Sulfuras (Artículo legendario):** No tiene fecha de venta y su calidad es siempre 80. No sufre cambios.  
- **Backstage passes (Entradas al Backstage):** Su calidad aumenta según la cercanía del concierto:
    - Más de 10 días: +1 de calidad.  
    - 10 días o menos: +2 de calidad.  
    - 5 días o menos: +3 de calidad.  
- **Después del concierto (SellIn < 0):** La calidad cae a 0.  
- **Conjured (Artículos conjurados):** Degradan su calidad el doble de rápido que los artículos normales (este es el ítem a implementar).  


### 2. ¿Qué significan SellIn y Quality y cuáles son sus límites?
- **SellIn:** Indica la cantidad de días que faltan para la fecha recomendada de venta del artículo. No tiene un límite superior o inferior definido, pero afecta el ritmo de cambio de la calidad cuando llega a 0.  
- **Quality:** Indica cuán valioso es el artículo.  
    - *Límite inferior:* Nunca puede ser menor a 0.  
    - *Límite superior:* No puede superar 50 (excepto para Sulfuras, que es fijo en 80).  
    
### 3. ¿Cuál es la restricción más importante sobre la clase Item?
La restricción crítica es que no se debe modificar la clase Item ni sus propiedades. Esta clase pertenece a un tercero (el "duende") y el sistema debe interactuar con ella tal como está, limitando los cambios únicamente a la clase GildedRose o extensiones del código.  

### 4. ¿Qué nuevo ítem hay que implementar al final del kata?
Se debe implementar el ítem "Conjured" (Conjurados), cuya característica principal es que su calidad se degrada el doble de rápido que la de un objeto normal. 


## Ficha de Análisis de Código Legacy: Gilded Rose
### 1. OLORES DE CÓDIGO (Code Smells) detectados:

**[X] Método demasiado largo:** El método update_quality tiene aproximadamente 38 líneas de pura lógica densa sin separaciones claras.  


**[X] Condicionales anidados:** Hay secciones con hasta 4 o 5 niveles de profundidad (por ejemplo, los if dentro del bloque de Backstage passes).  


**[X] Números mágicos:** Se usan literales como 11, 6, 50 y 0 directamente en la lógica sin explicar qué representan (como "Límite de calidad máxima" o "Umbral de urgencia").  


**[X] Strings literales repetidos:** Los nombres "Aged Brie", "Sulfuras, Hand of Ragnaros" y "Backstage passes to a TAFKAL80ETC concert" aparecen múltiples veces escritos a mano, lo que facilita errores de dedo.  


**[X] Falta de abstracción:** Toda la lógica de negocio para cada tipo de ítem diferente está mezclada en un solo bucle for, en lugar de estar delegada a clases o métodos específicos.  

**[ ] Variables con nombres poco descriptivos:** Aunque item, quality y sell_in son claros, la estructura lógica hace que sea difícil seguir qué está pasando.

**Otros:** 
- **Lógica Negada:** El uso constante de if item.name != ... hace que el código sea mucho más difícil de leer que si fuera positivo.

- **Código Muerto/Redundante:** La expresión item.quality = item.quality - item.quality es una forma muy extraña de decir item.quality = 0.


### 2. PUNTOS DE CAMBIO (Dónde añadir "Conjured"):
Tendría que ser dentro del for item in self.items:, probablemente cerca de la línea 10 o al final del método tras evaluar si sell_in < 0.  El riesgo de modificar ahí es debido a la alta anidación y las negaciones (!=), añadir un nuevo if puede romper accidentalmente la lógica de otros ítems o hacer que un ítem sea procesado por dos bloques distintos, duplicando la degradación de su calidad.  

### 3. DEPENDENCIAS:
- **¿Qué clases/structs usa GildedRose?** 
Depende totalmente de la clase Item.  

- **¿Quién llama a update_quality()?** 
En el contexto del código proporcionado, no se ve el llamador, pero el sistema indica que se ejecuta "al final de cada día" para actualizar el inventario.

### 4. PREGUNTAS SIN RESPUESTA en el código:
- **¿Qué pasa si quality es negativa al entrar?** 
El código actual solo resta calidad si item.quality > 0. Si entra un valor como -5, se quedaría en -5 (aunque los requisitos dicen que nunca debe ser negativa).  

- **¿Qué pasa con un nombre de ítem desconocido?** 
El código lo trata como un "Ítem Normal" por defecto debido a las estructuras if item.name != ....