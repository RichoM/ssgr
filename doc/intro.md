# Introduction to ssgr

TODO: write [great documentation](http://jacobian.org/writing/what-to-write/)

(defn sumar [a b]
    (+ a b))

(def a 3)

(def b 4)

(sumar a b)

## Título 2

Este es un texto que tiene [:strong "hiccup embebido"] en el medio del texto.
También puedo poner objetos interactivos como [:button {:onclick "alert('hola')"} "este botón"].

(def counter (atom 0))

Ahora voy a probar un contador.

El contador ahora tiene el valor (deref counter). Si lo incremento pasa a tener: (swap! counter inc).

(println "ACAACA")

Fin