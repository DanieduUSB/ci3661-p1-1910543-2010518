# IA.md

El equipo declara el uso de IA, particularmente la herramienta de Inteligencia Artificial: *Chat GPT*.

En lineas generales, la herramienta fue de utilidad para establecer bases fuertes para cada módulo, aclarar dudas puntuales, escribir o corregir código y sugerir cambios para documentación.

La herramienta utilizada **NO** es un agente, por lo tanto, todos los aportes fueron revisados por el equipo antes de agregarlos al archivo correspondiente.

## Motor.Tipos

Se utilizó al inicio para poder estructurar de manera mas clara el proyecto, pues los tipos son la base de todo. Si bien el aporte no fue muy significativo, la mayor utilidad fue para armar los errores de manera que mantuviera la división entre errores semánticos y sintácticos correctamente.

La herramienta sugirió distintas maneras de mostrar los errores: Cada uno (V1, V2..) por separado, o manejar ErrorSemantico y ErrorSintactico, etc. Al final se tomó la decisión actual tomando como base las ideas dadas.

También eventualmente generó las instancias Show que se usan para los debugs/tests. Esto para generar prints humanamente legibles rápidamente.

## Motor.Nucleo

Se usó mas que todo para una revisión de cada comando luego de ejecutarse (Prácticamente como la opinión de otra persona, mientras aún no existía la suite de pruebas).

Se pidieron opciones de implementación para los comandos Guardar y Cargar, las cuales al final fueron rechazadas porque se decidió implementar estos comandos en `Main.hs`

## Motor.Reconocedor

Se usó para corrección de errores, tomando como base la clase 5 para estas.

## Motor.Mundo

Se generaron algunas funciones en este módulo, con el fin de agilizar tiempo (Desafortunadamente es semana 8, por la complejidad del proyecto no podíamos darnos el lujo de escribir el 100% del código a mano)

## Motor.Serializador

Este es probablemente el archivo donde mas se usó la herramienta. Debido a que era un módulo extra que por lo general es una librería externa, se decidió pedir a la herramienta una alternativa, resultando en este archivo, pues, las librerías en el proyecto están limitadas.

## Suite Test

Las pruebas unitarias se generan con facilidad con estas herramientas, por lo que se usó en esta sección.

El mundo de prueba usado en casi todas las pruebas **NO** fue generado con IA, para evitar discrepancias con la información real, pero se buscó generar pruebas alrededor de este mundo.