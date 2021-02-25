INSTRUCCIONES PARA INICIAR EL ALGORITMO:

 


1) Desde el directorio que almacene los ficheros, en la terminal:
        
        * Ejecutar 'ghci Principal.hs'
          
          -> Si no funciona la librería I1M, se puede comentar “import I1M”
             y descomentar “import PilaConTipoDeDatoAlgebraico“ en
             FrozenLake.hs, Principal.hs y Algoritmo.hs.
    

 

2)En la consola interactiva:
        
        * Ejecutar las siguientes funciones:
          Main> resuelveJuego (iniciaEntorno 5 123)
                        donde 5 es <Tamaño matriz> y 123 es <semilla>

        ATENCIÓN: Con numeros demasiado elevados puede tardar mucho en cargar.