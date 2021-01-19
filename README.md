# TRABAJO PD

## Prerequisitos
Para crear el repositorio es necesario hacer clone del repositorio del proyecto, alojado en
https://alejandro_jimenez_martin@bitbucket.org/alejandro_jimenez_martin/trabajo_pd.git


## Forma de trabajo
Trabajermos mediante ramas para poder desarrollar distintas partes del trabajo en paralelo, y así
poder subir o bajar partes del código que no tengamos de manera inmediata.

La forma de trabajo que usaremos será la siguiente:
-> Rama master.
   En esta rama no trabajaremos, excepto para mergear el trabajo una vez finalizado y que funcione 
   correctamente (el merge se hará desde develop).

-> Rama develop.
   Desde esta rama crearemos nuestras ramas de trabajo personales, las que usaremos cada uno. 
   Aqui haremos los merges de nuestras ramas una vez hayamos terminado una serie de funciones 
   o código que sea importarte de compartir para el avance del otro compañero.
   Hay que tener mucho cuidado aqui, ya que al mergear ambas ramas debemos comprabar que todo 
   funciona correctamente.

-> Ramas personales.
   Cada uno de nosotros creará una rama (siempre desde develop) en la que trabajará y subirá su parte 
   de código que haya desarrollado, haciendo los correspondientes commits y pull cuando haya funciones 
   correctamente creadas y que funcionen a la perfección.
   Los merge a la rama develop sólo se harán cuando uno de los compañero necesite funciones o parte del 
   código, desarrolladas en la rama del otro compañero y que estén finalizadas y probadas correctamente, 
   para poder seguir avanzando.
   También podremos hacer un merge a develop entre ambos, cuando estemos en un punto intermedio de la 
   ejecución del proyecto para tener un punto de retorno que funciones correcatemente, por lo que pueda
   pasar (modificación de funciones que dejen de funcionar y afecten al proyecto, borrado de código sin
   querer, añadir nuevas funciones que funcionan pero afectan al correcto funcionamiento de otras, etc.)
   creando una etiqueta o tag para tenerlo controlado.


## Observaciones
Usaremos la platilla Principal.hs para la creación y desarrollo de los métodos principales del trabajo.

Si necesitaramos crear nuevo ficheros para métodos secundarios o para librerias, módulos,etc; podremos 
crearlos con toda libertad, previo acuerdo con el compañero para evitar confilctos a la hora de actualizar 
el repositorio.

Si se necesita crear directorios de carpetas dentro del proyecto para ubicar dichos ficheros mencionandos
anteriormente, podemos hacerlo libremente pero siempre acordandolo con el compañero, para evitar conflictos
a la hora de actualizar el repositorio.


## Bibliografía
Librería gym python:
https://gym.openai.com/

Teoría de matrices:
http://hackage.haskell.org/package/matrix-0.3.4.3/docs/Data-Matrix.html
http://www.cs.us.es/~jalonso/cursos/i1m/doc/manual-Data.Matrix.html

Juego FrozenLake:
https://github.com/openai/gym/blob/master/gym/envs/toy_text/frozen_lake.py

Ejercicios de haskell:
https://www.cs.us.es/~jalonso/cursos/i1m-10/ejercicios/ej_prog_Haskell.pdf
