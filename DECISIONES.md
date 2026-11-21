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

## D3. Organización y orden de los bloques del mundo

Autor: Daniela Gragirena (19-10543)

### Contexto

Para reconocer un mundo a partir de un archivo de texto, era necesario establecer cómo organizar la información correspondiente a la cabecera, las salas y los objetos.

El principal problema era decidir si los campos y bloques debían seguir un orden estricto o si se podía permitir cierta flexibilidad, sin dificultar el reconocimiento y la validación del mundo.

### Opciones

1. Establecer un orden estricto para todos los campos y bloques del archivo.
2. Permitir que todos los campos y bloques aparezcan en cualquier posición, sin restricciones.
3. Organizar el archivo en bloques, manteniendo algunas reglas de estructura, pero permitiendo flexibilidad en el orden de sus elementos.

### Decisión

Se escogió la opción 3. Se decidió organizar el archivo en bloques separados por `--`, donde el primero corresponde obligatoriamente a la cabecera y los siguientes pueden ser bloques de salas u objetos en cualquier orden.

Dentro de la cabecera, los campos `MUNDO`, `INICIO`, `META` y, opcionalmente, `CAPACIDAD` pueden aparecer en cualquier orden, siempre que pertenezcan al mismo bloque.

Para los bloques de salas y objetos, se estableció que su primera línea debe indicar el tipo de bloque mediante `SALA` u `OBJETO`, respectivamente. Los demás campos pueden aparecer en cualquier orden, respetando cuáles son obligatorios y cuáles pueden repetirse.

De esta forma, las definiciones de salas y objetos pueden intercalarse libremente, y dentro de una sala también pueden mezclarse las líneas `SALIDA` y `OBJETO`, sin necesidad de agruparlas.

### Consecuencias

Se obtiene un formato más flexible y sencillo de escribir, ya que no es necesario recordar un orden específico para todos los campos.

Se facilita la modificación de los mundos, permitiendo agregar salas, objetos y salidas sin reorganizar completamente el archivo.

El reconocimiento puede identificar cada bloque por su primera línea y procesar sus campos según sus nombres, independientemente del orden en que aparezcan.

## D4. Tratamiento de comentarios en los archivos del mundo

Autor: Daniela Gragirena (19-10543)

### Contexto

Los archivos del mundo pueden contener una cantidad considerable de información, especialmente cuando incluyen muchas salas, objetos y salidas. Por esta razón, resulta útil permitir comentarios que expliquen partes del archivo sin afectar su reconocimiento.

Inicialmente se contemplaba ignorar las líneas que comenzaran con el símbolo `#`. Sin embargo, también se consideró conveniente permitir comentarios al final de líneas que contienen información válida.

### Opciones

1. Permitir únicamente comentarios que ocupen una línea completa.
2. Permitir comentarios tanto en líneas independientes como al final de líneas válidas.

### Decisión

Se escogió la opción 2. Se decidió que todo el contenido ubicado después del símbolo `#` fuera ignorado durante la preparación de las líneas.

De esta manera, se pueden escribir comentarios independientes o agregar aclaraciones después de un campo válido, sin que el reconocedor intente interpretarlas como parte de su valor.

Esta funcionalidad se implementó en `prepararLineas`, dentro de `Motor.Mundo`, antes de separar el archivo en bloques. También se eliminan los espacios sobrantes y se ignoran las líneas vacías, conservando los números originales de las líneas para reportar errores.

### Consecuencias

Se permite agregar aclaraciones junto a las definiciones de salas, objetos y salidas.

El reconocimiento se simplifica porque los comentarios se eliminan antes de aplicar las comprobaciones sintácticas.

Como contrapartida, el símbolo `#` queda reservado para comentarios, por lo que no puede formar parte del texto de una descripción u otro valor, ya que todo lo escrito después de él será ignorado.

## D5. Cálculo de la capacidad mínima requerida

Autor: Daniela Gragirena (19-10543)

### Contexto

La variante Peso establece que cada objeto tiene un peso y que el jugador no puede transportar objetos cuya suma supere su capacidad.

Además de comprobar que ningún objeto individual supere la capacidad, era necesario decidir cómo determinar si la capacidad del jugador era suficiente para los objetos utilizados durante la aventura.

También se decidió permitir que el campo CAPACIDAD fuera opcional. En ese caso, el motor debía asignar automáticamente un valor que tuviera en cuenta los pesos de los objetos y los requisitos del mundo.

### Opciones

1. Establecer una capacidad predeterminada fija para todos los mundos.
2. Calcular la capacidad mínima exacta mediante una búsqueda de las posibles secuencias de acciones del jugador.
3. Utilizar una fórmula simplificada que considere el objeto más pesado y la suma de los pesos de todos los objetos necesarios para las salidas y la meta.

### Decisión

Se escogió la opción 3. Se decidió utilizar una comprobación conservadora para evitar implementar una búsqueda completa de soluciones, que habría aumentado considerablemente la complejidad del motor.

La capacidad requerida se calcula mediante la siguiente fórmula:

**Capacidad requerida = máximo(1, peso del objeto más pesado, suma de los pesos de los objetos necesarios).**

Se consideran objetos necesarios aquellos que aparecen como requisitos de alguna salida y, cuando la meta consiste en llevar un objeto a una sala, también el objeto de la meta.

Cada objeto necesario se cuenta una sola vez, aunque aparezca como requisito de varias salidas. Si un objeto no tiene un peso declarado, se considera que pesa 1.

Cuando el archivo incluye CAPACIDAD, se utiliza el valor indicado y se comprueba que sea suficiente según esta regla. Si el campo se omite, se asigna automáticamente la capacidad calculada. El valor mínimo permitido es 1.

### Consecuencias

Se garantiza que la capacidad calculada permite transportar cualquier objeto individual y, de ser necesario, todos los objetos considerados necesarios al mismo tiempo.

La fórmula es sencilla de implementar y no requiere explorar las distintas rutas ni las combinaciones posibles de acciones del jugador.

También permite que los mundos omitan el campo CAPACIDAD sin utilizar un valor fijo que podría resultar insuficiente.

Sin embargo, esta fórmula no representa necesariamente la capacidad mínima real para completar una aventura. Como el jugador puede soltar objetos y las salidas pueden permanecer desbloqueadas después de atravesarlas, existen mundos que pueden resolverse transportando los objetos necesarios en distintos momentos.

Por lo tanto, la validación puede rechazar algunos mundos que serían jugables con una capacidad menor. Se aceptó esta limitación a cambio de mantener una comprobación más conservadora.

## D6. Acumulación y clasificación de errores del mundo

Autor: Daniel

### Contexto

El motor debe informar los errores de un mundo sin detener la validación ante el primer problema. Además, conviene diferenciar los errores de formato de los problemas semánticos, como referencias a salas u objetos inexistentes, para ofrecer mensajes útiles y permitir su corrección.

### Opciones

1. Devolver un único error mediante `Either ErrorMundo Mundo`, deteniéndose al primer fallo.
2. Devolver únicamente mensajes de texto, sin tipos específicos de error.
3. Representar los errores mediante constructores de `ErrorMundo`, acumularlos en una lista y distinguir los errores sintácticos de los semánticos.

### Decisión

Se escogió la opción 3. `leerMundo` mantiene la firma `String -> Either [ErrorMundo] Mundo`. Se utiliza `ErrorSintactico Int ErrorV1` para asociar los errores de reconocimiento con su número de línea, y otros constructores de `ErrorMundo` para las comprobaciones semánticas. Las validaciones independientes reúnen sus errores antes de devolver el resultado final.

### Consecuencias

El usuario puede corregir varios errores de una sola vez y las pruebas pueden verificar cada categoría de error sin depender únicamente de mensajes literales. A cambio, hay que organizar cuidadosamente las dependencias entre comprobaciones: algunas validaciones no pueden ejecutarse de forma segura si todavía no se reconocieron los datos de los que dependen.

## D7. Separación entre reconocimiento sintáctico y validación del mundo

Autor: Daniela

### Contexto

El archivo del mundo combina reglas de sintaxis —claves, separadores, direcciones y metas— con restricciones semánticas que requieren conocer el conjunto de salas y objetos declarados. Mezclar ambas responsabilidades dificulta localizar fallos, reutilizar funciones y probarlas por separado.

### Opciones

1. Resolver reconocimiento y validación en una sola función extensa.
2. Separar los reconocedores de fragmentos de texto de las comprobaciones necesarias para construir y validar un mundo completo.

### Decisión

Se eligió la opción 2. `Motor.Reconocedor` concentra las funciones encargadas de interpretar las distintas estructuras del archivo mediante reconocedores `ReadS`, incluyendo líneas individuales y bloques completos de cabecera, salas y objetos.

Por su parte, `Motor.Mundo` prepara las líneas, organiza los bloques y coordina las validaciones sintácticas y semánticas. Primero comprueba la estructura de los bloques y sus campos, utilizando los reconocedores del otro módulo para validar sus valores. Posteriormente, construye el mundo completo y ejecuta las verificaciones de referencias, duplicados, alcanzabilidad y capacidad.

Los errores sintácticos conservan los números originales de las líneas para facilitar su identificación.

### Consecuencias

La separación permite probar los reconocedores individualmente sin necesidad de construir un mundo completo, mientras que las validaciones globales permanecen centralizadas en `Motor.Mundo`.

Además, facilita distinguir los errores sintácticos de los semánticos y evita que los reconocedores individuales deban conocer todas las entidades declaradas en el mundo.

Como contrapartida, ambos módulos están relacionados: una modificación del formato de entrada puede requerir cambios en los reconocedores y en las reglas de validación de `Motor.Mundo`.

## D8. Desbloqueo permanente e independiente de salidas

Autor: Daniel

### Contexto

Las salidas pueden exigir un objeto para atravesarlas. Era necesario decidir si ese requisito debe volver a cumplirse en cada cruce y qué sucede con la salida de regreso. La decisión también afecta la variante Peso, porque conservar permanentemente llaves y herramientas ocupa capacidad de inventario.

### Opciones

1. Exigir el objeto cada vez que se atraviese una salida.
2. Desbloquear permanentemente todas las conexiones entre dos salas tras el primer cruce.
3. Desbloquear permanentemente solo la salida dirigida que se atravesó con éxito.

### Decisión

Se escogió la opción 3. `Estado` mantiene `salidasDesbloqueadas :: Set (IdSala, Direccion)`. La función `puedeCruzar` de `Motor.Nucleo` permite el paso cuando la salida no tiene requisito, el jugador lleva el objeto correspondiente o la salida ya está desbloqueada. El desbloqueo se registra al cruzar exitosamente una salida que tenía requisito; no se desbloquea automáticamente el sentido contrario.

### Consecuencias

El jugador puede soltar objetos que ya utilizó para abrir rutas y así liberar capacidad. El modelo representa con precisión el sentido de cada salida, incluso cuando las conexiones no son bidireccionales.

Los desbloqueos forman parte del estado dinámico y deben conservarse al guardar una partida.

## D9. Separación de operaciones de archivos, serialización y lógica del juego

Autor: Daniel

### Contexto

Los comandos normales se ejecutan mediante `ejecutar :: Comando -> Estado -> ([String], Estado)`, una función pura. Sin embargo, leer mundos y guardar o cargar partidas requiere operaciones `IO`. Además, convertir el estado a texto no implica necesariamente acceder al sistema de archivos.

### Opciones

1. Incorporar `IO` directamente a `Motor.Nucleo` y cambiar la firma de `ejecutar`.
2. Conservar un núcleo puro y concentrar lectura, escritura y conversión de datos en un único módulo.
3. Separar la lectura/escritura de archivos de la serialización y dejar que `Main` coordine los comandos con efectos externos.

### Decisión

Se eligió la opción 3. `Motor.Persistencia` expone `leerArchivo :: FilePath -> IO (Either String String)` y `escribirArchivo :: FilePath -> String -> IO (Either String ())`. `Motor.Serializador` convierte entre `Estado` y `String` mediante funciones puras. `Main` es responsable de coordinar `Guardar` y `Cargar`, mientras `Motor.Nucleo` permanece dedicado a las transiciones puras del juego.

### Consecuencias

La lógica puede probarse sin crear archivos y el manejo de errores de entrada/salida queda separado del reconocimiento de mundos y partidas.

Las funciones de persistencia no determinan si el contenido es válido: esa decisión corresponde a los módulos que interpretan el texto.

`Main` requiere manejar explícitamente los resultados de cada operación.