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