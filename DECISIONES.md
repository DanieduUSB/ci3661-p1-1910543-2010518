# DECISIONES.md

## Registro de decisiones de arquitectura

Este documento presenta las principales decisiones de diseño tomadas durante el desarrollo del motor de aventuras de texto en Haskell.

## D1. Cómo se representa el mundo y los identificadores

Autor: Daniel Quijada (20-10518)

### Contexto

El motor necesita almacenar y consultar salas, objetos y salidas mediante identificadores, al igual que debe detectar identificadores duplicados durante la validación del mundo, sin perder información antes de reportar los errores.

Se tenía bastante claro como identificar el mundo en sí: un data con distintos atributos como título, inicio (id de la sala), meta, lista de salas y lista de objetos (y posteriormente se añadió capacidad del jugador), el problema era cómo representar las listas correspondientes de manera eficiente, con facilidad de acceso, y facilidad para comprobar una construcción de mundo válida.

### Opciones

1. Representar salas y objetos como listas de su respectivo objeto.
2. Representar salas y objetos como conjuntos de su respectivo objeto.
2. Utilizar un Map que recibiera el ID de la sala u objeto y retornara el respectivo objeto.
3. Utilizar un Map que recibiera el ID de la sala u objeto y retornara una lista del respectivo objeto.

### Decisión

Se tomó la opción 3. Esta estructura está pensada no solo para representar el mundo, sino verificarlo al momento de armarlo. Esto se podía hacer con listas, pero las listas son mas ineficientes que los Maps al tratarse de búsqueda, y con Sets no se podían detectar repetidos.

### Consecuencias

La verificación de repetidos de salas y objetos es O(1)

Las búsquedas son O(1) igualmente

El tipo no garantiza por si solo que todo lo interno respete el formato solicitado, se debe verificar en Motor.Mundo

Ya con un mundo armado válido, cuando se busca una sala u objeto por su ID, la función retorna una lista de Sala u Objeto, por lo que hay que considerar que acceder al elemento específico es una función mas.

## D2. Qué guarda el Estado

Autor: Daniel Quijada (20-10518)

### Contexto

Durante una partida, el jugador puede realizar distintas acciones que afectan tanto los datos del jugador como de su entorno. La pregunta mas importante en este punto es: ¿Vale la pena mantener los datos originales, o es mejor modificar estos datos y desechar completamente lo anterior?

### Opciones

1. Crear una copia de las salas y objetos al inicio de la partida y después de cada cambio sin preservar lo anterior

2. Mantener una copia del mundo y cambiar solamente la información necesaria

### Decisión

Se optó por la opción 2. Básicamente no tiene sentido una separación de tipos si ambos representan virtualmente lo mismo, si Estado copia el mundo entero en sí mismo. Es mas fácil y rápido de implementar un tipo de Dato Estado que simplemente registre los cambios necesarios.

Por lo tanto se implementó data Estado con los atributos:

- estadoMundo: Mundo con su configuración inicial
- salaActual: ID de la sala donde se encuentra el jugador
- inventario: Lista de IDs de los objetos del jugador
- objetosPorSala: Map cuya clave es el ID de la sala y el valor un conjunto con los objetos. Como los objetos no se repiten, el conjunto es ideal
- salidasDesbloqueadas: Se mencionará este atributo en particular en un ADR posterior, pues corresponde a una decisión tomada más adelante
- juegoTerminado: Booleano que indica si el juego terminó o no

### Consecuencias

Se evita modificar la estructura original del mundo y se simplifica la ejecución de comandos puros.

Por ejemplo, ejecutar (Tomar idObj) elimina el objeto de objetosPorSala y lo incorpora al inventario. Soltar realiza la operación contraria.

Esto también permite que un objeto sea soltado en una sala distinta de su ubicación original.

Como contrapartida, es necesario mantener la consistencia entre inventario y salas para evitar objetos duplicados o desaparecidos.
