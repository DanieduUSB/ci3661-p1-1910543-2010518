# ci3661-p1-1910543-2010518
Proyecto #1 Laboratorio de Lenguajes de Programación (CI3661) Trimestre Sep-Dic 2026.

El motor de texto se implementó con la variante **PESO**

El proyecto utiliza `Stack` como gestor de paquetes y compilador.

## Integrantes

- Daniela Gragirena 19-10543
- Daniel Quijada 20-10518

## Compilación

Compilar y generar el ejecutable `aventura` en la carpeta principal del proyecto.

```bash
stack build --copy-bins --local-bin-path .
```

## Ejecución

```bash
./aventura MUNDO
./aventura MUNDO < GUION
```

MUNDO y GUION corresponden a rutas relativas desde la carpeta principal del proyecto de archivos de texto.

## Pruebas

```bash
stack test
```

Se espera:

```
aventura> test (suite: aventura-test)

Cases: 117  Tried: 117  Errors: 0  Failures: 0



aventura> Test suite aventura-test passed

```