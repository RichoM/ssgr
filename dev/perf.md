
(+ 3 4)

(ssgr/markdown "**Texto en negrita**

1. First item
2. Second item")

---

*Texto en cursiva*

1. First item
2. Second item: (ssgr/markdown "[link](https://richo.itch.io)")

Richo

1. item one
2. item two
   - sublist
     que continúa en la siguiente línea.

     Y que además tiene otro párrafo.
   - sublist

*****Hello*world****

foo\
bar

Un párrafo con un `code span
que pasa a la línea siguiente` y luego termina...

Texto normal en una línea.
Texto normal en otra línea.

Otro párrafo.


Un tercer párrafo con un `code span` y un [link](url).

Un cuarto párrafo, ahora con clojure (+ 3 4)



(def fig-counter (atom 0))

(defn fig [text]
 [:div "Fig. " (swap! fig-counter inc)
       " - " text ])

# Introduction to ssgr

TODO: write [great documentation](http://jacobian.org/writing/what-to-write/)

(defn sumar [a b]
    (+ a b))

(def a 3)

(def b 4)

(sumar a b)


(fig "Este es una imagen")

(fig "Otra figura en el medio")


(fig "esta es otra imagen")

<iframe frameborder="0" src="https://itch.io/embed/2567646" width="552" height="167"><a href="https://richo.itch.io/retro-racing-dd">Retro Racing: Double Dash by Richo</a></iframe>
[:iframe {:src "https://itch.io/embed/2567646"
:width 552 :height 167}
[:a {:href "https://richo.itch.io/retro-racing-dd"} "Retro racing"]]

## Título 2

Este es un texto que tiene [:strong "hiccup embebido"] en el medio del texto.
También puedo poner objetos interactivos como [:button {:onclick "alert('hola')"} "este botón"].

(def counter (atom 0))

Ahora voy a probar un contador.

El contador ahora tiene el valor (deref counter). Si lo incremento pasa a tener: (swap! counter inc).



1. item one
   - sublist
     * sub sub list
   - sublist


I need to buy
- new shoes
- a coat
- a plane ticket

# Taller de Robocup Rescue

[Unidad 3: Primeros pasos](03/README.md)

[Unidad 4: Sensado avanzado](04/README.md)

[Unidad 5: Procesamiento de imágenes](05/README.md)

*(*foo*)*

*(**foo**)*



# Heading 1
## Heading 2

Párrafo con 1 línea.

Párrafo con 2 líneas.
Esta es la 2da línea.

---

Heading 2 (setext)
---
Heading 1 (settext)
=

___
    Código indentado.
    Con varias líneas...

```python
# Código python:
3 + 4
````

~~~
Fenced code block without closing fence




# 1. Estructura general del programa

Para poder programar el comportamiento del robot es necesario escribir un archivo con código Python. Este archivo, llamado comúnmente “controlador”, tiene una serie de requisitos que debemos cumplir para que funcione correctamente.

En primer lugar, es necesario “importar” algunas clases importantes del espacio de nombres “controller”. Este espacio de nombres incluye clases útiles para poder controlar los motores y acceder a los valores de los sensores. Pero lo más importante de todo es que en este espacio de nombres se incluye la clase “Robot”, que necesitamos para interactuar con la simulación.

```python
from controller import Robot
```

Sin embargo, con sólo importar la clase Robot no alcanza. Tenemos que crear,  también, una instancia de Robot para poder llamar a los métodos correspondientes.

Lo más conveniente es entonces crear una instancia de Robot y guardarla en una variable, cómo se ve a continuación:

```python
robot = Robot()
```

De esta forma, cada vez que necesitemos interactuar con el simulador podemos aprovechar la variable “robot” para enviar un mensaje a la instancia de “Robot” que acabamos de crear. Este objeto nos servirá también para poder obtener los objetos que representan a los distintos dispositivos que posee el robot (motores y sensores).

Incluso más importante que acceder a los dispositivos del robot es el método “step”, que nos permite delegar el control de la ejecución al simulador para que éste lleve a cabo todas las acciones necesarias para avanzar la simulación. 

Si no ejecutamos el método “step” a intervalos regulares, entonces, la simulación no avanza ya que durante la ejecución de “step” el simulador actualiza los valores de los sensores y actuadores además de realizar todos los cálculos de posición, velocidad, y colisiones de cada objeto simulado.

Más información de la función “step” en: [https://cyberbotics.com/doc/guide/controller-programming?tab-language=python#the-step-and-wb_robot_step-functions](https://cyberbotics.com/doc/guide/controller-programming?tab-language=python#the-step-and-wb_robot_step-functions)

Por lo tanto, una vez que tenemos creada nuestra instancia de Robot y podemos mandarle mensajes, el siguiente paso es ejecutar el ciclo de la simulación, usualmente de la siguiente forma:

```python
while robot.step(32) != -1:
    pass # Reemplazar por la lógica del controlador
```

El método “step” espera como argumento un múltiplo de la cantidad de milisegundos que tarda un paso de simulación. El parámetro de la función step especifica la cantidad de tiempo, expresada en milisegundos, que debe simularse hasta que la función step() regrese, es decir, el número de milisegundos entre actualizaciones del mundo. 

Como los pasos de simulación no se pueden interrumpir, la medición de un sensor o el accionamiento de un motor ocurre entre dos pasos de simulación. Debido a eso, por ejemplo, si el paso de simulación es de 16 ms, el argumento que se pasa a robot.step() debe ser un múltiplo de ese valor. Puede ser 16, 32, 64, 128, etc.

Dado que el valor del paso de simulación es una constante que vamos a necesitar más adelante para inicializar los sensores, es recomendable almacenarlo en una variable (usualmente llamada “TIME_STEP”).

Entonces, el código mínimo necesario para programar un controlador válido en webots tiene la siguiente forma:

```python
from controller import Robot

TIME_STEP = 32
robot = Robot()

while robot.step(TIME_STEP) != -1:
    pass # Reemplazar por la lógica del controlador
```

Por supuesto, este código no hace absolutamente nada más que invocar el ciclo de la simulación. Para controlar el robot de forma que tenga algún comportamiento interesante es necesario reemplazar el código dentro del “while” con la lógica que queremos darle al robot.

[Descargar ejemplo completo](01_EstructuraGeneral/ejemplo_1.py)

# 2. Uso de la consola

La consola es una herramienta muy útil para mostrar información de la ejecución del programa que a veces no es directamente visible en el simulador.

Podemos usar la consola tanto para mostrar mensajes cuando el programa pasa por un camino de ejecución determinado, como también para  visualizar información relacionada al estado del programa (valores de las variables, valores de los sensores, cálculos intermedios, etc.).

Como se podrán imaginar, todo esto resulta de vital importancia a la hora de depurar nuestros programas (es decir, de diagnosticar y arreglar errores). Por lo tanto, aprender a usar la consola de forma efectiva resulta fundamental para el desarrollo de programas sofisticados.

Además de mostrar los mensajes que define el usuario en su programa controlador, la consola también muestra los errores de compilación y los mensajes provenientes del simulador. De esta forma, si tenemos, por ejemplo, un error de sintaxis en nuestro programa vamos a ver un mensaje de error en la consola indicándonos el problema y usualmente ese mensaje de error es el primer paso hacia una solución del problema.

De forma predeterminada, la consola muestra todos los registros de Webots y las salidas del controlador. Sin embargo, la consola posee un menú contextual que permite al usuario crear un filtro para definir qué registros y salidas del controlador se quieren mostrar (opción “Filter” del menú).

![](02_UsoDeConsola/imgs/image-0.png)

De forma predeterminada, solo hay una consola disponible, pero puede ser útil usar más de una consola (especialmente cuando se filtra lo que se muestra en cada una). Se puede abrir una nueva consola desde el menú “Tools” o desde el menú contextual de una consola ya existente.

![](02_UsoDeConsola/imgs/image-1.png)

## Ejemplo 1 - Cadena de texto

La forma más elemental de mostrar mensajes en la consola es usando la función “print” pasando una cadena de texto como parámetro:

```python
print("Hola mundo")
```

![](02_UsoDeConsola/imgs/image-2.png)

[Descargar ejemplo 1 completo](02_UsoDeConsola/ejemplo_1.py)

## Ejemplo 2 - Flujo de ejecución

Usar cadenas de texto como mensajes en la consola puede parecer poco útil pero en realidad es una forma fácil de analizar el flujo de ejecución del programa, sobre todo cuando no estamos seguros de si una parte del código está ejecutándose correctamente o no. La utilidad de esta técnica se puede apreciar mejor en programas más complejos pero en este ejemplo sencillo se puede ver como algunas partes del código se ejecutan una vez, otras se ejecutan varias veces, y algunas no se ejecutan en ningún momento:

```python
print("Inició la simulación")

while robot.step(TIME_STEP) != -1:
    if True:
        print("Se está ejecutando la simulación")
    else:
        print("Este mensaje NO debería aparecer")

print("Terminó la simulación")
```

![uso de la [consola](consola.com)](02_UsoDeConsola/imgs/image-3.png)

[Descargar ejemplo 2 completo](02_UsoDeConsola/ejemplo_2.py)

## Ejemplo 3 - Variables

Probablemente la forma más común de usar la consola sea para mostrar el valor de alguna variable (o varias). En este caso, en lugar de pasar una cadena de texto como parámetro de la función “print” lo que pasamos es directamente el valor que queremos mostrar en la consola. 

En el ejemplo siguiente se observa cómo mostrar en la consola los segundos desde que inició la simulación:

```python
print(robot.getTime()) # Muestro los segundos actuales en la consola
```

![](02_UsoDeConsola/imgs/image-4.png)

[Descargar ejemplo 3 completo](02_UsoDeConsola/ejemplo_3.py)

## Ejemplo 4 - "Formatted string"

Cuando empezamos a mostrar múltiples valores en la consola resulta fácil confundir el significado de cada valor. Resulta entonces muy útil combinar una cadena de texto con el valor que queremos mostrar de forma que podamos identificar qué significa cada valor:

```python
c = 0 # Usamos esta variable para contar los ciclos de simulación

while robot.step(TIME_STEP) != -1:
    c = c + 1 # En cada ciclo, incrementamos el contador en 1
    print(f"Cantidad de ciclos: {c}")
    print(f"Segundos de simulación: {robot.getTime()}")
```

![](02_UsoDeConsola/imgs/image-5.png)

[Descargar ejemplo 4 completo](02_UsoDeConsola/ejemplo_4.py)

## Ejemplo 5 - Condiciones

Puede resultar útil mostrar un mensaje en la consola dependiendo de si se cumple o no una condición. El siguiente ejemplo muestra en la consola un mensaje cada 10 segundos:

```python
t0 = robot.getTime() # Guardamos los segundos al iniciar el programa

while robot.step(TIME_STEP) != -1:
    # Chequeamos si pasaron 10 segundos.
    if robot.getTime() - t0 > 10:
        print("A") # Ya pasaron 10 segundos
        t0 = robot.getTime() # Vuelvo a iniciar el contador
```

![](02_UsoDeConsola/imgs/image-6.png)

[Descargar ejemplo 5 completo](02_UsoDeConsola/ejemplo_5.py)

# 3. Control de los motores

Para controlar los motores debemos seguir los siguientes pasos:

1. Obtener un objeto Motor para cada motor que vayamos a utilizar. Es conveniente guardar este objeto en una variable de forma que podamos usarlo más adelante para referirnos al mismo.
2. Establecer la posición de cada motor en infinito para que los mismos puedan girar libremente, sin detenerse en una posición determinada.
3. Establecer la velocidad a la que queremos que el motor gire (en [radianes](https://es.wikipedia.org/wiki/Radi%C3%A1n) por segundo)


> NOTA: Los radianes son una forma de medir ángulos al igual que los grados sexagesimales. Podemos convertir entre radianes y grados usando las siguientes fórmulas:
>
> * radianes = grados * PI / 180
> * grados = radianes * 180 / PI


Para obtener cada objeto Motor debemos ejecutar el método `robot.getDevice` pasando como parámetro el nombre del motor:

* Para el motor izquierdo: “wheel1 motor”
* Para el motor derecho: “wheel2 motor”

## Ejemplo 1 - Prendiendo motores

En el siguiente ejemplo se pueden observar los 3 pasos antes mencionados para mover el motor izquierdo a la máxima velocidad permitida.

```python
from controller import Robot

TIME_STEP = 32
MAX_VEL = 6.28 # Velocidad máxima (1 vuelta por segundo)

robot = Robot()

wheelL = robot.getDevice("wheel1 motor") # Paso 1 
wheelL.setPosition(float("inf")) # Paso 2

while robot.step(TIME_STEP) != -1:
    wheelL.setVelocity(MAX_VEL) # Paso 3
```

La velocidad máxima de cada motor es 6,28 radianes/s, lo cual equivale a una vuelta completa cada segundo. Si superamos la velocidad máxima webots muestra un error en la consola, por lo cual recomendamos usar una variable global (MAX_VEL, en este caso) para guardar la velocidad máxima y poder referirnos a ella a la hora de establecer la velocidad final del motor.

[Descargar ejemplo 1 completo](03_Motores/ejemplo_1.py)

## Ejemplo 2 - Avanzar

Para mover el robot en el mapa, sin embargo, no alcanza con controlar el motor izquierdo, necesitamos ambos motores. Dependiendo de la velocidad que asignemos a cada motor vamos a obtener diferentes tipos de movimientos.

Para avanzar, por ejemplo, debemos asignar la misma velocidad a ambos motores:

```python
wheelL.setVelocity(MAX_VEL)
wheelR.setVelocity(MAX_VEL)
```

[Descargar ejemplo 2 completo](03_Motores/ejemplo_2.py)

## Ejemplo 3 - Retroceder

Para retroceder usamos la misma velocidad en ambos motores, pero en sentido inverso (nótese el signo):

```python
wheelL.setVelocity(-MAX_VEL)
wheelR.setVelocity(-MAX_VEL)
```

[Descargar ejemplo 3 completo](03_Motores/ejemplo_3.py)

## Ejemplo 4 - Girar a la derecha

Si queremos hacer que el robot gire, vamos a asignar diferentes velocidades a cada motor. El tipo de giro más sencillo que podemos realizar es una rotación sobre el centro del robot. Para ello, asignamos la misma velocidad a cada motor pero en uno de los dos invertimos el signo (es decir, hacemos que uno de los dos motores gire en sentido contrario al otro). Dependiendo del sentido de giro que deseamos obtener, invertiremos uno u otro motor.

Para rotar en sentido horario (o hacia la derecha, visto desde la perspectiva del robot) debemos entonces invertir el sentido del motor derecho:

```python
wheelL.setVelocity(MAX_VEL)
wheelR.setVelocity(-MAX_VEL)
```

[Descargar ejemplo 4 completo](03_Motores/ejemplo_4.py)

## Ejemplo 5 - Girar a la izquierda

Por el contrario, si deseamos girar en sentido anti-horario (o hacia la izquierda), invertiremos el motor izquierdo:

```python
wheelL.setVelocity(-MAX_VEL)
wheelR.setVelocity(MAX_VEL)
```

[Descargar ejemplo 5 completo](03_Motores/ejemplo_5.py)

## Ejemplo 6 - Uso de los encoders

Los motores del robot e-puck incluyen también encoders que permiten determinar cuánto giró el motor. Para acceder a los valores de los encoders debemos seguir los siguientes pasos:

1. Obtener el objeto encoder del motor. Para ello usamos el método “motor.getPositionSensor”.
2. Habilitar el encoder usando el método “enable”. Este método espera un parámetro que debe ser la frecuencia de actualización del encoder (usualmente TIME_STEP).
3. Obtener el valor de rotación del encoder (en radianes). Importante: girar el motor hacia atrás decrementa este valor.

En el siguiente ejemplo habilitamos el encoder del motor izquierdo y mostramos en la consola su posición mientras el motor gira a un 10% de su velocidad máxima.

```python
from controller import Robot

TIME_STEP = 32
MAX_VEL = 6.28 # Velocidad máxima (1 vuelta por segundo)

robot = Robot()

wheelL = robot.getDevice("wheel1 motor") 
wheelL.setPosition(float("inf"))

encoderL = wheelL.getPositionSensor() # Paso 1
encoderL.enable(TIME_STEP) # Paso 2

while robot.step(TIME_STEP) != -1:
    wheelL.setVelocity(0.1*MAX_VEL)

    pos = encoderL.getValue() # Paso 3
    print(f"La posición del motor es {pos} radianes")
```

[Descargar ejemplo 6 completo](03_Motores/ejemplo_6.py)

---
## Ejercicios

Para cada ejercicio se pide armar un programa controlador distinto y entregar los archivos de código.

1. Prender ambos motores a la mitad de la velocidad máxima.
2. Avanzar una distancia determinada y luego volver a la posición inicial.
3. Girar 90 grados en sentido horario y luego frenar.
4. Girar 180 grados en sentido antihorario y luego frenar.
5. Trazar un círculo con el movimiento del robot. IMPORTANTE: Usar el mundo "mapa_circulito.wbt"


# 4. Sensores de distancia

Los sensores infrarrojos del robot sirven para medir la distancia al primer objeto en la línea de visión del sensor. Están configurados para funcionar en el rango entre 0 y 0,8 metros.

Para acceder los sensores de distancia se deben seguir 3 pasos:
1. Obtener el sensor ejecutando el método “robot.getDevice” y pasando como parámetro el nombre del dispositivo (por defecto: “ps0” a “ps7”)
2. Habilitar el sensor usando el método “enable” y pasando como parámetro la frecuencia de actualización del sensor (este valor usualmente es igual al TIME_STEP configurado anteriormente)
3. Finalmente, obtener los valores del sensor usando el método “getValue”

## Ejemplo 1 - Activando el sensor

En el siguiente ejemplo se pueden ver los 3 pasos necesarios para obtener el valor del sensor “ps0” y mostrarlo en la consola.

```python
from controller import Robot

TIME_STEP = 32

robot = Robot()

ps0 = robot.getDevice("ps0") # Paso 1
ps0.enable(TIME_STEP) # Paso 2

while robot.step(TIME_STEP) != -1:
    dist = ps0.getValue() # Paso 3
    print(f"Distancia: {dist}")
```

Si bien este programa no mueve el robot podemos moverlo manualmente usando los controles de webots y así observar cómo los valores registrados por el sensor van cambiando a medida que lo acercamos o alejamos de la pared.

![](04_Distancia/imgs/image-1.png)

En el robot por defecto los sensores de distancia están distribuidos alrededor del robot en posiciones predeterminadas.

![](04_Distancia/imgs/image-2.png)

Tenemos que tener en cuenta la ubicación de cada sensor para decidir cuáles habilitar e incluso, dependiendo de la estrategia elegida, podría ser útil modificar la ubicación de los sensores para tener información más precisa.

[Descargar ejemplo 1 completo](04_Distancia/ejemplo_1.py)

## Ejemplo 2 - Avanzar hasta pared

Los sensores de distancia son muy útiles para detectar la cercanía del robot con las paredes u obstáculos, lo cual nos va a permitir comenzar a diseñar un sistema de navegación que tenga en cuenta el entorno del robot.

Podemos implementar muy fácilmente, por ejemplo, un robot que avance hasta detectar la cercanía de un obstáculo y luego se detenga. Habilitamos en este caso “ps0” y “ps7”, que son los dos sensores delanteros, de forma que podamos detectar cuando el robot se aproxime a la pared. Luego, en cada ciclo de la simulación chequeamos el valor de ambos sensores y si alguno devuelve un valor menor a 0,06 entonces detenemos ambos motores, de lo contrario avanzamos.

```python
ps7 = robot.getDevice("ps7")
ps7.enable(TIME_STEP)

ps0 = robot.getDevice("ps0")
ps0.enable(TIME_STEP)

while robot.step(TIME_STEP) != -1:
    # Si detectamos una pared, nos detenemos
    if ps7.getValue() < 0.06 or ps0.getValue() < 0.06:
        wheelL.setVelocity(0)
        wheelR.setVelocity(0)
    else:
        wheelL.setVelocity(MAX_VEL)
        wheelR.setVelocity(MAX_VEL)
```

[Descargar ejemplo 2 completo](04_Distancia/ejemplo_2.py)

## Ejemplo 3 - Mantener distancia

Los sensores de distancia pueden servir para programar comportamientos más interesantes. En el siguiente ejemplo podemos observar un robot que (con cierta torpeza) se mueve manteniendo la distancia con la pared izquierda. 

> NOTA: Recomendamos para este ejemplo utilizar los mundos [mapa_cuadrado_1.wbt](04_Distancia/mapa_cuadrado_1.wbt) y [mapa_cuadrado_2.wbt](04_Distancia/mapa_cuadrado_2.wbt)

![](04_Distancia/imgs/image-3.png)

En este caso, los sensores que nos interesan son “ps6” y “ps5”, dado que ambos están a la izquierda del robot. Lo que hace el programa es simplemente registrar en una variable llamada “delta0” la diferencia entre los dos sensores en el primer ciclo de la simulación y luego, en los ciclos siguientes, intenta corregir el movimiento del robot de forma que la diferencia entre ambos sensores se mantenga en valores cercanos a la medición inicial. Para ello, simplemente sustrae “delta0” a la diferencia actual y usa el valor resultante para decidir hacia dónde debe moverse el robot: si el valor es positivo significa que el robot se alejó demasiado de la pared, y si es negativo significa que está demasiado cerca.

```python
delta0 = None # Declaramos la variable delta0

while robot.step(TIME_STEP) != -1:
    if delta0 == None: # Sólo en el primer ciclo, inicializamos delta0
        delta0 = ps6.getValue() - ps5.getValue()
    
    # Calculamos la diferencia con la medición inicial
    giro = (ps6.getValue() - ps5.getValue()) - delta0
    
    # Avanzamos dependiendo del signo de "giro"
    # giro positivo = izquierda
    # giro negativo = derecha
    if giro > 0:
        wheelL.setVelocity(0.5*MAX_VEL)
        wheelR.setVelocity(1.0*MAX_VEL)
    else:
        wheelL.setVelocity(1.0*MAX_VEL)
        wheelR.setVelocity(0.5*MAX_VEL)

    # Si nos acercamos mucho a la pared, giramos rápidamente
    if ps6.getValue() < 0.06:
        wheelL.setVelocity(1.0*MAX_VEL)
        wheelR.setVelocity(-1.0*MAX_VEL)
```

[Descargar ejemplo 3 completo](04_Distancia/ejemplo_3.py)

## Ejemplo 4 - Activando todos los sensores

Es muy probable que en el controlador final necesitemos usar todos los sensores de distancia que posee el robot. En ese caso, la opción más conveniente para escribir la menor cantidad de código es guardarlos en una lista en lugar de usar variables individuales. Luego, cuando queremos chequear el valor de un sensor simplemente tenemos que buscarlo por su índice en la lista. De esta forma, el sensor ps0 se encontraría en el índice 0 y el ps7 en el índice 7.

```python
distSensors = []
for i in range(8):
    sensor = robot.getDevice("ps" + str(i))
    sensor.enable(TIME_STEP)
    distSensors.append(sensor)

while robot.step(TIME_STEP) != -1:
    for i in range(8):
        print(f"Distancia sensor {i}: {distSensors[i].getValue()}")
    print("================")
```

Puede ser útil en algunos casos visualizar los rayos proyectados por cada sensor en el mapa como se puede vera continuación.

![](04_Distancia/imgs/image-4.png)

Para habilitar esta visualización debemos hacer click en el menú de webots “View” → “Optional Rendering” y habilitar la opción “Show DistanceSensor Rays”

![](04_Distancia/imgs/image-5.png)

[Descargar ejemplo 4 completo](04_Distancia/ejemplo_4.py)

---
## Ejercicios

Para cada ejercicio se pide armar un programa controlador distinto y entregar los archivos de código.

1. Leer el valor de uno de los sensores de distancia e imprimirlo por consola.
2. Activar más de un sensor de distancia e imprimir sus valores por consola.
3. Avanzar hasta encontrar una pared y frenar. Usar [mapa_pasillo.wbt](04_Distancia/mapa_pasillo.wbt)
4. Girar hasta detectar el hueco alrededor del robot. Usar [mapa_salida.wbt](04_Distancia/mapa_salida.wbt)


# Recorriendo un mapa básico sin agujeros

La estrategia más conocida para recorrer un laberinto es aquella que nos indica que debemos mantener siempre una de nuestras manos (ya sea la izquierda o la derecha) pegada a la pared. Esta estrategia es conocida como el algoritmo “[wall follower](https://en.wikipedia.org/wiki/Maze-solving_algorithm#Wall_follower)” (o método de la mano izquierda/derecha), y es muy fácil de implementar.

Podemos aplicar esta estrategia en nuestro robot aprovechando los [sensores de distancia](04_Distancia.md) para mantener al robot siempre “pegado” (a una distancia prudente) a alguna de las paredes.

En este ejemplo usaremos 2 [sensores de distancia](04_Distancia.md): ps5 y ps7. Por un lado, necesitamos detectar la pared izquierda para poder mantener la distancia, para eso usaremos el sensor ps5. Necesitamos también detectar las paredes al frente del robot (para evitar chocar), para eso usaremos el sensor ps7.

![](05_Mapa/imgs/image-1.png)

Recomendamos, para este ejemplo, usar el mundo “[mapa_noholes_1.wbt](05_Mapa/mapa_noholes_1.wbt)” ya que este mapa está armado de tal forma que no contenga agujeros ni trampas y además permite observar el comportamiento del robot ante diferentes tipos de giros.

![](05_Mapa/imgs/image-2.png)

Lo primero que hacemos entonces es inicializar el robot y sus dispositivos (los dos motores y dos sensores).

```python
robot = Robot()

wheelL = robot.getDevice("wheel1 motor")
wheelL.setPosition(float("inf"))

wheelR = robot.getDevice("wheel2 motor")
wheelR.setPosition(float("inf"))

ps7 = robot.getDevice("ps7")
ps7.enable(TIME_STEP)

ps5 = robot.getDevice("ps5")
ps5.enable(TIME_STEP)
```

Vamos a introducir en este ejemplo una función “delay” que resultará muy útil para esperar un tiempo determinado sin detener la ejecución de la simulación. Esta función ejecuta el “robot.step” en un loop hasta que haya transcurrido el tiempo deseado. De esta forma, el controlador del robot no realiza ninguna acción mientras se ejecuta el loop pero la simulación no se detiene.

```python
def delay(ms):
    initTime = robot.getTime()
    while robot.step(TIME_STEP) != -1:
        if (robot.getTime() - initTime) * 1000.0 >= ms:
            break
```

Aprovechando la función “delay” vamos a implementar dos funciones que nos permitan girar a la derecha y a la izquierda, respectivamente. Estas funciones establecen la velocidad de las ruedas y luego esperan un tiempo determinado para que el giro que realice el robot tenga la forma deseada. Una vez realizado el giro, frenamos los motores antes de continuar con la ejecución.

```python
def turnRight():
    wheelL.setVelocity(MAX_VEL)
    wheelR.setVelocity(-MAX_VEL)
    delay(350)
    wheelL.setVelocity(0)
    wheelR.setVelocity(0)
    delay(1)

def turnLeft():
    wheelL.setVelocity(0.30*MAX_VEL)
    wheelR.setVelocity(1.00*MAX_VEL)
    delay(350)
    wheelL.setVelocity(0)
    wheelR.setVelocity(0)
    delay(1)
```

Dentro del loop principal vamos a realizar tres acciones. Primero, capturamos los valores de los sensores y los guardamos en variables para poder acceder a los mismos fácilmente. Estos valores representan la distancia hasta la pared izquierda y la distancia hacia la pared frontal.

```python
# Leo los sensores de distancia y guardo los valores en variables
dist_left = ps5.getValue()
dist_front = ps7.getValue()
```

Luego, en función de la distancia a la pared izquierda ajustamos la velocidad de las ruedas. El objetivo es avanzar lo más recto posible, pero si detectamos la cercanía con la pared entonces reducimos ligeramente la velocidad de la rueda derecha de forma que el robot se aleje lentamente de la pared. De forma similar, si detectamos que nos estamos alejando demasiado de la pared, reducimos la velocidad de la rueda izquierda. Por último, si la distancia hasta la pared se mantiene dentro del umbral arbitrario entre 0,035 m y 0,045 m hacemos avanzar ambas ruedas a la máxima velocidad.

```python
# Ajustamos la velocidad de las ruedas dependiendo de la distancia
# con la pared izquierda. Si nos acercamos mucho giramos ligeramente
# a la derecha. Si nos alejamos giramos ligeramente a la izquierda.
# El objetivo es mantenernos en un rango entre 0.035 y 0.045
if dist_left < 0.035:
    wheelL.setVelocity(1.0 * MAX_VEL)
    wheelR.setVelocity(.95 * MAX_VEL)
elif dist_left > 0.045:
    wheelL.setVelocity(.95 * MAX_VEL)
    wheelR.setVelocity(1.0 * MAX_VEL)
else:
    wheelL.setVelocity(1.0 * MAX_VEL)
    wheelR.setVelocity(1.0 * MAX_VEL)
```

El paso final de la estrategia es reaccionar correctamente a los cruces y giros que presente el mapa. En particular nos interesa responder a dos tipos de situaciones: un giro a la izquierda y un giro obligado a la derecha.

Como podemos observar en la siguiente imagen, el giro a la izquierda es fácilmente reconocible con sólo mirar el sensor ps5. Si el sensor no detecta la presencia de una pared, independiente del valor del sensor ps7, podemos saber con seguridad que se aproxima un giro a la izquierda. Por el contrario, para detectar el giro a la derecha necesitamos que ambos sensores indiquen la presencia de una pared. 

![](05_Mapa/imgs/image-3.png)

```python
# Si la distancia a la pared izquierda supera los 0.1 metros
# significa que tenemos que girar 90 grados hacia la izquierda.
# Si en cambio nos encontramos con una pared adelante, tenemos
# que girar 90 grados a la derecha.
if dist_left > 0.1:
    turnLeft()
elif dist_front < 0.05:
    turnRight()
```

Un problema con esta estrategia es que, dependiendo de la estructura del laberinto puede haber partes del mismo que nos queden sin explorar. Por ejemplo, la zona marcada en amarillo en la siguiente figura. Recordemos que el objetivo del robot no es encontrar la salida del laberinto sino explorarlo en su totalidad.

![](05_Mapa/imgs/image-4.png)

¿Cómo podemos modificar la estrategia para que el robot pueda explorar todo el mapa?

[Descargar ejemplo completo](05_Mapa/ejemplo_1.py)

---
## Ejercicios

Para cada ejercicio se pide armar un programa controlador distinto y entregar los archivos de código.

1. Desarrollar un robot que recorra el mapa y estacione en la primer baldosa que encuentre rodeada por 3 paredes. Usar el mundo "[mapa_parking.wbt](05_Mapa/mapa_parking.wbt)"
2. Desarrollar un robot que recorra todas las zonas del mapa "[mapa_noholes_1.wbt](05_Mapa/mapa_noholes_1.wbt)"

# Unidad 3: Primeros pasos

En esta unidad comenzaremos a dar nuestros primeros pasos en la programación del robot. 

Para ello deberemos primero entender la estructura básica que debe tener un programa controlador, dado que usaremos esta misma estructura en todos los ejemplos y ejercicios. 

Luego, aprenderemos a usar la consola de webots para mostrar información útil de nuestro programa. 

A continuación, procederemos a familiarizarnos con algunos de los componentes esenciales del robot: los motores y sensores de distancia. 

Una vez podamos usar de forma efectiva estos componentes empezaremos a desarrollar algunas estrategias sencillas de navegación que nos permitirán más adelante explorar completamente el mapa. 

Finalmente, aprenderemos a construir mapas personalizados para utilizar en nuestras pruebas.

[1. Estructura general del programa](01_EstructuraGeneral.md)

[2. Uso de la consola](02_UsoDeConsola.md)

[3. Control de los motores](03_Motores.md)

[4. Sensores de distancia](04_Distancia.md)

# 1. Posicionamiento del robot

El robot cuenta con 4 sensores que podemos utilizar para estimar su posición y orientación en el mapa:

1. El acelerómetro
2. El giroscopio
3. La unidad de medición inercial
4. El GPS

El acelerómetro y el giroscopio nos brindan información relativa al movimiento del robot (en particular, aceleración y velocidad angular), la unidad de medición inercial nos entregan información acerca de la orientación del robot, y el GPS nos permite obtener la posición absoluta del robot mediante coordenadas en los 3 ejes (X, Y, Z).

![](01_Posicionamiento/imgs/image-1.png)

En este tutorial vamos a aprender a controlar estos 3 sensores para hacernos una idea precisa de la posición y orientación del robot, de forma que podamos utilizar luego esta información para mejorar la exploración del mapa.

## Ejemplo 1 - Acelerómetro

El acelerómetro nos permite obtener el valor de aceleración del robot (variación de velocidad en unidad de tiempo) en los 3 ejes del robot (X, Y, Z).

Como con todos los componentes del robot, tenemos que seguir los 3 pasos clásicos para utilizar el sensor, con una sola diferencia: dado que el acelerómetro devuelve 3 valores en lugar de sólo 1, debemos ejecutar el método “getValues” (nótese el plural) en lugar de “getValue”.

1. Obtener el objeto que representa al sensor mediante el mensaje “robot.getDevice”. El nombre del sensor en el robot por defecto es “accelerometer”.
2. Habilitar el sensor enviando el mensaje “enable” pasando como parámetro el TIME_STEP de la simulación.
3. Acceder a los valores del sensor mediante el mensaje “getValues”.

En este ejemplo lo que vamos a hacer es introducir una nueva función “updateVars()”, encargada de actualizar las variables que consideremos importantes para la lógica del controlador. En particular, para este caso sólo la usaremos para obtener la información de la aceleración y mostrarla en la consola.

```python
# Creamos el objeto accelerometer y lo habilitamos
accel = robot.getDevice("accelerometer")
accel.enable(TIME_STEP)

# En esta variable vamos a guardar los valores que obtenemos del sensor
acceleration = [0, 0, 0]

def updateVars():
    global acceleration
    acceleration = accel.getValues()
    x, y, z = acceleration
    print(f"X: {x:.3f} m/s^2")    
    print(f"Y: {y:.3f} m/s^2")
    print(f"Z: {z:.3f} m/s^2")
    print("================")
```

Dado que el valor de los sensores cambian conforme avanza la simulación es importante llamar a “updateVars()” luego de cada “robot.step()”, de esta forma podemos estar seguros que la variable “acceleration” tiene siempre la información actualizada. En nuestro ejemplo, tenemos una llamada a “robot.step()” en el loop principal y otra llamada en la función “delay”.

```python
def delay(ms):
    initTime = robot.getTime()
    while robot.step(TIME_STEP) != -1:
        updateVars() # Llamamos a updateVars() luego de robot.step()
        if (robot.getTime() - initTime) * 1000.0 > ms:
            break

while robot.step(TIME_STEP) != -1:
    updateVars() # Llamamos a updateVars() luego de robot.step()
```

Finalmente, para poder observar la aceleración del robot tenemos que moverlo:

```python
    wheelL.setVelocity(0.25*MAX_VEL)
    wheelR.setVelocity(0.25*MAX_VEL)
    delay(500)

    wheelL.setVelocity(0)
    wheelR.setVelocity(0)
    delay(500)

    wheelL.setVelocity(1.0*MAX_VEL)
    wheelR.setVelocity(1.0*MAX_VEL)
    delay(500)

    wheelL.setVelocity(0)
    wheelR.setVelocity(0)
    delay(500)
```

[Descargar ejemplo 1 completo](01_Posicionamiento/ejemplo_1_accelerometer.py)

## Ejemplo 2 - Giroscopio

El giroscopio nos permite obtener la velocidad angular del robot. Así como con el acelerómetro, debemos seguir los 3 pasos habituales:

1. Obtener el objeto que representa al giroscopio mediante el mensaje “robot.getDevice”. El nombre del sensor en el robot por defecto es “gyro”.
2. Habilitar el sensor enviando el mensaje “enable” pasando como parámetro el TIME_STEP de la simulación.
Acceder a los valores del sensor mediante el mensaje “getValues”
3. Si bien el sensor nos devuelve la información de la velocidad angular para los 3 ejes de rotación del robot (X, Y, Z) el eje que nos resulta más útil es el eje vertical (cualquier rotación en otro sentido implicaría que el robot está tumbado).

Del mismo modo que con el acelerómetro, el giroscopio devuelve los valores para cada sensor en una tupla de 3 elementos. Sin embargo, dependiendo de la orientación del sensor en el robot utilizado, el valor que nos interesa puede estar ubicado en cualquier posición de la tupla. Podemos verificar esto muy rápidamente haciendo un controlador que gire el robot sobre sí mismo y observando cuál de los 3 valores del sensor cambia. En el caso del robot por defecto, se verifica que el valor que nos interesa se ubica en el índice 1 de la tupla.

El valor de velocidad angular que devuelve el giroscopio está representado en radianes por segundo y, mediante un [cálculo muy sencillo](https://www.texasgateway.org/resource/61-angle-rotation-and-angular-velocity), podemos transformar este valor en una estimación de la orientación actual del robot (lo cual nos será muy útil para mejorar la navegación).

Como siempre, comenzamos por habilitar el giroscopio:

```python
# Inicializamos el giroscopio
gyro = robot.getDevice("gyro")
gyro.enable(TIME_STEP)
```

Así como hicimos con el acelerómetro, vamos a declarar un conjunto de variables globales que nos servirán para almacenar el valor de la orientación del robot. En este caso, para realizar el cálculo necesitaremos saber también el tiempo que pasó entre cada medición del sensor (es decir, entre cada ciclo de la simulación), por lo tanto también utilizaremos variables para guardar esa información.

```python
# Esta variable va a tener la orientación del robot (en radianes).
rotation = 0

# Necesitamos algunas variables más para llevar la cuenta del tiempo de la 
# simulación, y cuánto tiempo pasó desde el último ciclo
beginTime = robot.getTime()
currentTime = beginTime
deltaTime = 0
```

A continuación, modificaremos la función “updateVars()” para calcular cuánto giró el robot desde el último ciclo y sumarlo a la variable “rotation”.

```python
# La función updateVars() se encarga de actualizar las variables globales 
# de acuerdo a los valores de los sensores. 
# IMPORTANTE: Hay que llamarla después de cada robot.step()
def updateVars():
    global currentTime, deltaTime, rotation
    # Primero calculamos cuánto tiempo pasó desde el último ciclo
    lastTime = currentTime
    currentTime = robot.getTime()
    deltaTime = currentTime - lastTime
    
    # Luego calculamos la rotación del robot:
    # 1) Obtenemos primero la velocidad angular
    _, vel, _ = gyro.getValues()
    # 2) Calculamos luego la rotación en el último ciclo y la sumamos a la 
    # variable rotation
    rotation += (vel * deltaTime)
    # 3) Normalizamos el valor de rotation para que se mantenga siempre entre
    # 0 y 360 grados (o el equivalente en radianes: 0 y 2*PI)
    rotation %= math.tau # Normalizamos el valor del ángulo
    
    # OPCIONAL: Calcular el valor de rotación en grados y mostrarlo en consola
    degrees = rotation * 180/math.pi
    print(f"Velocidad: {vel:.3f} rad/s")
    print(f"Rotación: {rotation:.3f} rad ({degrees:.3f} deg)")
    print("================")
```

Es importante llamar a la función “updateVars” luego de cada “paso” de la simulación, por eso en este ejemplo decidimos encapsular el llamado a la función “robot.step()” en una función “step” propia que llame a “updateVars” automáticamente. Así nos aseguramos que los valores de las variables estén siempre actualizadas y no perdamos precisión en el cálculo de la orientación.

```python
# Encapsulamos la llamada a robot.step() en una función step() propia que llama 
# automáticamente a updateVars(). De esta forma evitamos llamar a updateVars() 
# manualmente porque step() lo hace por nosotros.
def step():
    result = robot.step(TIME_STEP)
    updateVars()
    return result
```

Ahora que tenemos nuestra propia función “step” es necesario reemplazar en el código en todos los lugares donde llamábamos a “robot.step” por una llamada a nuestra función. En este caso, los únicos dos lugares son la función “delay” y el loop principal.

```python
# Tenemos que actualizar delay() para que llame a nuestra función step() en
# lugar de robot.step()
def delay(ms):
    initTime = robot.getTime()
    while step() != -1:
        if (robot.getTime() - initTime) * 1000.0 > ms:
            break

# En lugar de llamar a robot.step() llamamos a nuestra función step()
while step() != -1:
    wheelL.setVelocity(-0.25*MAX_VEL)
    wheelR.setVelocity(0.25*MAX_VEL)
    delay(500)
    wheelL.setVelocity(0)
    wheelR.setVelocity(0)
    delay(500)
```

Finalmente, en el loop principal hacemos girar al robot para verificar que el cálculo de la orientación es correcto.

[Descargar ejemplo 2 completo](01_Posicionamiento/ejemplo_2_gyro.py)

## Ejemplo 3 - Unidad de medición inercial (Inertial Unit o IMU)

La unidad de medición inercial ("inertial unit" en inglés) es un dispositivo electrónico que funciona detectando la actual tasa de aceleración usando uno o más acelerómetros, y detecta los cambios en atributos rotacionales tales como cabeceo ("pitch"), alabeo ("roll") y guiñada ("yaw") usando uno o más giróscopos.

![](01_Posicionamiento/imgs/image-2.jpg)

En nuestro caso, podemos utilizar el inertial unit en lugar del giroscopio para obtener la rotación del robot.

> IMPORTANTE: Desafortunadamente, el robot por defecto no incluye un inertial unit, por lo cual en este ejemplo usaremos el siguiente robot personalizado: [robot_inertial_unit.json](01_Posicionamiento/robot_inertial_unit.json)

Los pasos para utilizar el inertial unit son los siguientes:

1. Obtener el objeto que representa al inertial unit mediante el mensaje “robot.getDevice”. El nombre del sensor en el [robot_inertial_unit.json](01_Posicionamiento/robot_inertial_unit.json) es “inertial_unit”.
2. Habilitar el sensor enviando el mensaje “enable” pasando como parámetro el TIME_STEP de la simulación.
3. Acceder a los valores del inertial unit mediante el mensaje "getRollPitchYaw".

```python
# Inicializamos la unidad de medición inercial
inertialUnit = robot.getDevice("inertial_unit")
inertialUnit.enable(TIME_STEP)
```

Así como en los ejemplos anteriores, utilizaremos la función "updateVars" para obtener los valores de los sensores y actualizar una variable global que represente la orientación del robot.

```python
# Esta variable va a tener la orientación del robot (en radianes)
rotation = None

# La función updateVars() se encarga de actualizar las variables globales 
# de acuerdo a los valores de los sensores. 
# IMPORTANTE: Hay que llamarla después de cada robot.step()
def updateVars():
    global rotation
    
    # 1) Obtenemos la rotación del robot en los tres ejes
    roll, pitch, yaw = inertialUnit.getRollPitchYaw()    
    print(f"Roll: {roll:.3f}, Pitch: {pitch:.3f}, Yaw: {yaw:.3f}")

    # 2) El eje que nos interesa para la rotación del robot es el yaw
    rotation = yaw

    # 3) Normalizamos el valor de rotation para que se mantenga siempre entre
    # 0 y 360 grados (o el equivalente en radianes: 0 y 2*PI)
    rotation %= math.tau # Normalizamos el valor del ángulo

    # OPCIONAL: Calcular el valor de rotación en grados y mostrarlo en consola
    degrees = rotation * 180/math.pi
    print(f"Rotación: {rotation:.3f} rad ({degrees:.3f} deg)")
    print("================")
```

Para asegurarnos de tener siempre la variable actualizada definiremos una función "step" que ejecute "updateVars" automáticamente luego de llamar a "robot.step".

```python
# Encapsulamos la llamada a robot.step() en una función step() propia que llama 
# automáticamente a updateVars(). De esta forma evitamos llamar a updateVars() 
# manualmente porque step() lo hace por nosotros.
def step():
    result = robot.step(TIME_STEP)
    updateVars()
    return result
```

Por supuesto, también tenemos que asegurarnos de llamar a "step" en lugar de "robot.step" en el resto del programa. Particularmente en la función "delay" y en el loop principal.

```python
# Tenemos que actualizar delay() para que llame a nuestra función step() en
# lugar de robot.step()
def delay(ms):
    initTime = robot.getTime()
    while step() != -1:
        if (robot.getTime() - initTime) * 1000.0 > ms:
            break

# En lugar de llamar a robot.step() llamamos a nuestra función step()
while step() != -1:
    wheelL.setVelocity(-0.25*MAX_VEL)
    wheelR.setVelocity(0.25*MAX_VEL)
    delay(500)
    wheelL.setVelocity(0)
    wheelR.setVelocity(0)
    delay(500)
```

[Descargar ejemplo 3 completo](01_Posicionamiento/ejemplo_3_inertialunit.py)


## Ejemplo 4 - GPS

El GPS nos permite obtener la posición absoluta del robot en el mundo. Como tal, este sensor resulta de vital importancia a la hora de la exploración del mapa ya que nos permite ir registrando, mientras el robot se mueve, qué baldosas fueron visitadas y qué características pudo obtener el robot acerca de las mismas. Como veremos más adelante, poder hacer esto de forma precisa es fundamental tanto para el trazado del mapa como para desarrollar estrategias de exploración más sofisticadas.

Para usar el GPS tenemos que seguir los 3 pasos habituales:

1. Obtener el objeto que representa al GPS mediante el mensaje “robot.getDevice”. El nombre del sensor en el robot por defecto es “gps”.
2. Habilitar el sensor enviando el mensaje “enable” pasando como parámetro el TIME_STEP de la simulación.
3. Acceder a los valores del GPS mediante el mensaje “getValues”.

```python
gps = robot.getDevice("gps") # Paso 1: Obtener el sensor
gps.enable(TIME_STEP) # Paso 2: Habilitar el sensor

while robot.step(TIME_STEP) != -1:
    # Paso 3: Usar el método getValues() para obtener la posición del robot (x, y, z)
    x, y, z = gps.getValues()
    
    print(f"X: {x:.3f}, Y: {y:.3f}, Z: {z:.3f}")
```

[Descargar ejemplo 4 completo](01_Posicionamiento/ejemplo_4_gps.py)

---
## Ejercicios

Para cada ejercicio se pide armar un programa controlador distinto y entregar los archivos de código.

1. Programar una función "girar" que reciba el ángulo a girar y use el valor del inertial unit para decidir cuándo detenerse (tratar de que el movimiento sea lo más preciso y rápido posible)
2. Programar una función "avanzar" que reciba la distancia a recorrer y use el valor del GPS para calcular cuándo detenerse (tratar de que el movimiento sea lo más preciso y rápido posible)
3. Usar las nuevas funciones de movimiento para mejorar el algoritmo de navegación desarrollado en la unidad anterior (girar de forma precisa y moverse de centro de baldosa a centro de baldosa)

# 2. Detección del color del piso

El sensor de color de piso que posee el robot e-puck es, en realidad, una cámara RGB que captura una imagen de un pixel de ancho por un pixel de alto.

Usando la información de la cámara podemos identificar los diferentes tipos de baldosas que forman parte del mapa. En particular, nos interesa detectar:

* Pantanos
* Agujeros
* Conexiones entre áreas

Como cada tipo de baldosa tiene un color particular, podemos comparar la información obtenida por el sensor con los colores de las baldosas especiales y así determinar el tipo de baldosa sobre el que se encuentra el sensor.

Para usar el sensor de color tenemos que seguir los 3 pasos habituales:

1. Obtener el objeto que representa al sensor mediante el mensaje “robot.getDevice”. El nombre del sensor en el robot por defecto es “colour_sensor”.
2. Habilitar el sensor usando el mensaje “enable” pasando como parámetro el TIME_STEP de la simulación.
3. Acceder a los valores observador por el sensor mediando el mensaje “getImage”.

## Ejemplo 1 - Descomponer la imagen en canales RGB

Dado que el sensor de color es realmente una cámara, tenemos que utilizar el método “getImage” para acceder a la imagen que está observando el sensor. Esta imagen contiene únicamente 1 pixel así que podemos descomponerlo fácilmente en los canales RGB correspondientes.

La función “getImage()” devuelve un array con la información de cada píxel de la imagen. Cada pixel está compuesto por 4 bytes correspondientes a los canales azul (B), verde (G), rojo (R), y alpha (A). Dado que la imagen contiene sólo 1 pixel, nos interesan los primeros 3 valores del array (el canal A, que indica el grado de transparencia del píxel, lo ignoramos). 

```python
# Obtener el sensor de color y habilitarlo
colorSensor = robot.getDevice("colour_sensor")
colorSensor.enable(TIME_STEP)

while robot.step(TIME_STEP) != -1:
    # Acceder al color detectado por el sensor. El canal A lo ignoramos.
    b, g, r, a = colorSensor.getImage()

    print(f"R: {r}, G: {g}, B: {b}")
```

[Descargar ejemplo 1 completo](02_ColorPiso/ejemplo_1.py)

## Ejemplo 2 - Detección de pantanos

Una vez que podemos obtener el color detectado por el sensor en sus tres canales RGB resulta trivial usar este valor para identificar el tipo de baldosa que detecta el sensor.

Una forma sencilla consiste en apoyar el sensor sobre la baldosa a identificar para tomar nota del color de la misma. Es importante no guiarse únicamente por la imagen que muestra el simulador sino leer el valor observado por el sensor. Haciendo esto podemos verificar que el color del pantano es cercano a (R:244, G:221, B:141).

Luego, es sólo cuestión de comparar el valor detectado por el sensor con el que tenemos identificado como pantano. Es conveniente usar un umbral en la comparación para asegurarnos que la detección funcione incluso ante diferencias en la iluminación que modifiquen ligeramente el color del piso. 

```python
# Obtener el sensor de color y habilitarlo
colorSensor = robot.getDevice("colour_sensor")
colorSensor.enable(TIME_STEP)

def esPantano(r, g, b):
    # El color del pantano es (R:244, G:221, B:141), así que analizamos cada 
    # canal por separado y usamos un umbral para comparar.
    return abs(r - 244) < 15 \
        and abs(g - 221) < 15 \
        and abs(b - 141) < 15

while robot.step(TIME_STEP) != -1:
    # Acceder al color detectado por el sensor. El canal A lo ignoramos.
    b, g, r, a = colorSensor.getImage()

    # Si llegamos a un pantano, mostramos un mensaje
    if esPantano(r, g, b):
        print(f"{robot.getTime():.2f}: Ojo! Pantano!")
```

[Descargar ejemplo 2 completo](02_ColorPiso/ejemplo_2.py)

---

## Ejercicios

Para cada ejercicio se pide armar un programa controlador distinto y entregar los archivos de código.

1. Identificar los distintos tipos de baldosa e imprimir en consola el nombre (NO los valores de RGB!)
2. Hacer un robot que recorra el mapa [easy1.wbt](02_ColorPiso/easy1.wbt) y NO se caiga en agujeros.

# 3. Uso del LIDAR

El LIDAR es un sensor avanzado que nos permite obtener una representación casi tridimensional de los objetos que rodean al robot. Funciona emitiendo un láser alrededor del robot y calculando la distancia desde el emisor hasta la primera obstrucción del láser.

Para usar el LIDAR debemos armar un robot personalizado y agregar el sensor, ya que no viene incluido en el robot por defecto. Es un sensor que nos brinda mucha información con un alto grado de precisión, por lo tanto, es muy costoso de agregar al robot. Y también es costoso en términos de performance en la ejecución. Por lo tanto, si decidimos incluirlo en nuestro robot debemos integrarlo muy bien en nuestra estrategia.

En todos los ejemplos siguientes vamos a utilizar un robot por defecto que pueden descargar en el siguiente link: [robot_lidar.json](03_LIDAR/robot_lidar.json)

Si se desea armar un robot personalizado debemos utilizar la [herramienta de personalización de robots](https://v24.robot.erebus.rcj.cloud/). 

> IMPORTANTE: La posición por defecto en la que la herramienta de personalización ubica el sensor es en el centro del robot. Deberemos, por lo tanto, modificar la ubicación y orientación del LIDAR para que pueda detectar los objetos que lo rodean. De lo contrario, la información obtenida no tendrá ninguna utilidad.

Para usar el LIDAR en el código del controlador debemos seguir los habituales 3 pasos:

1. Obtener el objeto que representa al sensor mediante el mensaje “robot.getDevice”. El nombre del LIDAR en robot_lidar.json es “lidar”.
2. Habilitar el sensor enviando el mensaje “enable” pasando como parámetro el TIME_STEP de la simulación.
3. Acceder a la información del sensor mediante alguno de los métodos que implementa el objeto Lidar.

Los primeros dos pasos son iguales al resto de los sensores. Sin embargo, el tercer paso, la obtención de los datos del sensor, puede hacerse de varias maneras.

La opción más sencilla consiste en solicitarle al LIDAR la información de la profundidad detectada para cada emisión del láser. En este caso, la información que nos entrega el sensor es una lista de números dónde cada valor representa la distancia detectada por el sensor a medida que el láser escanea los alrededores del robot. El sentido del escaneo es primero de izquierda a derecha y luego de arriba a abajo.

La siguiente opción, un poco más sofisticada, es habilitar en el LIDAR la generación de una nube de puntos. Esencialmente, esta nube de puntos representa la misma información que la imagen de profundidad pero cada valor está transformado en un punto tridimensional relativo al centro del sensor. Generar esta nube de puntos es computacionalmente costoso, por lo cual puede ralentizar el funcionamiento del simulador.

Adicionalmente, para saber cómo interpretar la información que entrega el sensor (independientemente del método elegido) necesitamos más datos. En particular, necesitamos saber la resolución horizontal (es decir, cuántos puntos de izquierda a derecha detecta el sensor) y la cantidad de capas (cuántas filas de puntos desde arriba hacia abajo). Con esta información podemos luego analizar los valores del sensor.

## Ejemplo 1 - Habilitar el sensor y obtener su información básica

Vamos a comenzar por habilitar el sensor y solicitar la información básica que necesitamos del sensor.

```python
# Obtenemos el objeto que representa al sensor y lo habilitamos
lidar = robot.getDevice("lidar") # Paso 1
lidar.enable(TIME_STEP) # Paso 2
```

A continuación, lo único que hacemos en el loop principal es solicitar la información deseada al sensor usando los métodos apropiados en cada caso: para la resolución horizontal el método “getHorizontalResolution”, para la cantidad de capas “getNumberOfLayers”, y para la imagen de profundidad “getRangeImage”.

```python
# Paso 3 - Obtenemos la información del sensor:
# - horizontalResolution: Cuantos puntos de izquierda a derecha
# - numberOfLayers: Cuántas filas de arriba a abajo
# - rangeImage: La imagen de profundidad
horizontalResolution = lidar.getHorizontalResolution()
numberOfLayers = lidar.getNumberOfLayers()
rangeImage = lidar.getRangeImage()
```

Finalmente, mostramos los datos obtenidos en la consola.

```python
# Finalmente mostramos estos datos en la consola
    print(rangeImage)
    print(f"Resolución horizontal (ancho): {horizontalResolution}")
    print(f"Cantidad de capas (alto): {numberOfLayers}")
    print("=====================")
```

Como se puede observar a continuación, la resolución del sensor es 512 x 4.

![](03_LIDAR/imgs/image-1.png)

[Descargar ejemplo 1 completo](03_LIDAR/ejemplo_1.py)

## Ejemplo 2 - Visualización de la imagen de profundidad

Cuando trabajamos con datos de cierta complejidad suele ser conveniente visualizarlos de manera que podamos hacernos una idea completa de la información y entenderla con mayor facilidad.

Lamentablemente, como pudimos observar en el ejemplo anterior, la naturaleza de la información que entrega el LIDAR hace que sea imposible de visualizar usando sólo la consola.

Por esta razón, vamos a utilizar la librería de procesamiento de imagen OpenCV para transformar la información que entrega el sensor en una imagen que podamos observar.

OpenCV es una librería muy poderosa para realizar sofisticados análisis y transformaciones de imágenes. Sin embargo, en esta oportunidad sólo vamos a utilizar su funcionalidad más elemental: la visualización de las imágenes. Más adelante, veremos algunas de las funciones más avanzadas de OpenCV.

En primer lugar, necesitamos importar algunas librerías extra. En particular, NumPy para la transformación de los datos y OpenCV para la visualización.

```python
# Vamos a usar NumPy para transformar la data del sensor en una imagen 
# y OpenCV para mostrarla en la pantalla
import numpy as np
import cv2
```

Para el procesamiento de los datos vamos a necesitar un par de funciones utilitarias: “partition” para particionar una secuencia en listas de un tamaño específico, y “flatten” para aplanar una lista de listas.

```python
# Función para particionar una secuencia en listas de un tamaño máximo
def partition(seq, length):
    chunks = []
    chunk = []
    for e in seq:
        chunk.append(e)
        if len(chunk) == length:
            chunks.append(chunk)
            chunk = []
    if len(chunk) > 0:
        chunks.append(chunk)
    return chunks

# Función para "aplanar" una lista de listas
def flatten(t):
    return [item for sublist in t for item in sublist]
```

Dentro del loop principal obtenemos la imagen de profundidad y la guardamos en una variable para procesarla luego.

```python
# Obtenemos la imagen
image = lidar.getRangeImage()
```

A continuación, necesitamos transformar la información de la profundidad en algo que podamos visualizar. En este caso, usaremos escala de grises. A mayor la distancia detectada más blanco el color visualizado. Adicionalmente, necesitamos “estirar” la imagen verticalmente, dado que el sensor captura sólo 4 capas. Para estirar, simplemente repetimos cada capa 32 veces.

```python
# Convertimos la información de profundidad en pixeles en escala de grises
# y al mismo tiempo estiramos la imagen para que tenga 64 pixeles de alto
pixels = []
for d in flatten([p*32 for p in partition(image, 512)]):
    color = d * 255
    color = int(max(min(color, 255), 0))
    pixels.append(color)
```

Luego, invocamos funciones de NumPy para convertir la lista de píxeles en una imagen que pueda mostrar OpenCV.

```python
# Convertimos el array de pixeles en una imagen
img = np.frombuffer(bytes(pixels), np.uint8).reshape((4*32, 512))
```

Y por último, usamos la función “imshow” de OpenCV para mostrar la imagen en la pantalla. Necesitamos también ejecutar “waitKey” para renderizar la imagen.

```python
# Mostramos la imagen en la pantalla
cv2.imshow("lidar", img)
cv2.waitKey(1)
```

![](03_LIDAR/imgs/image-2.png)

[Descargar ejemplo 2 completo](03_LIDAR/ejemplo_2.py)

## Ejemplo 3 - Generación de la nube de puntos

Además de poder visualizar la información, suele ser útil en algunas ocasiones exportar los datos a un archivo para procesarlos luego usando alguna herramienta especializada.

En este ejemplo aplicaremos esta técnica para exportar la nube de puntos que genera el LIDAR en [formato CSV](https://es.wikipedia.org/wiki/Valores_separados_por_comas) para luego poder analizarlos usando una aplicación de hojas de cálculo (como Excel).

Primero que nada, vamos a utilizar una librería para que se encargue de escribir el archivo en el [formato CSV](https://es.wikipedia.org/wiki/Valores_separados_por_comas). De esta forma, nos ahorramos tener que lidiar con los detalles del formato ya que la librería lo hace por nosotros.

```python
import csv # Librería para escribir y leer archivos CSV
```

Para definir en qué carpeta vamos a escribir el archivo con la nube de puntos introduciremos otra constante llamada “FOLDER”. 

```python
# En esta carpeta vamos a grabar los datos
FOLDER = "X:\\"
```

> IMPORTANTE: Cambiar el valor de esta cadena para que tenga sentido la ubicación en el sistema de archivos propio.

La generación de la nube de puntos debe ser habilitada antes de poder usarla, para eso enviamos el mensaje “enablePointCloud” al LIDAR.

```python
# Para calcular la nube de puntos tenemos que habilitarla primero 
lidar.enablePointCloud()
```

Luego, ya dentro del loop principal, obtenemos la nube de puntos mediante el mensaje “getPointCloud” y la guardamos en una variable “points”

```python
# Obtenemos la nube de puntos
points = lidar.getPointCloud()
```

Este método nos devuelve una lista de puntos donde cada elemento es un objeto de tipo LidarPoint. Cada uno de estos objetos representa un punto en 3 dimensiones detectado por el LIDAR. Para poder escribir el archivo, necesitamos transformar estos objetos en secuencias que contengan los atributos deseados en el orden en el que los vamos a escribirlos en el archivo. En este caso los atributos son: “x”, “y”, “z”, “layer”, y “time”. Luego de la transformación, guardamos la información nuevamente en la variable “points”.

```python
# Cada punto es un objeto con los siguientes atributos x/y/z/layer/time,
# necesitamos transformarlos en una secuencia 
points = [(p.x, p.y, p.z, p.layer, p.time) for p in points]
```

Finalmente, escribimos el archivo en el disco. El nombre del archivo será “point_cloud.csv” y estará ubicado en la carpeta especificada anteriormente en la constante “FOLDER”.

```python
# Finalmente, escribimos el archivo
with open(FOLDER + "/point_cloud.csv", "w", newline="") as f:
    writer = csv.writer(f)
    writer.writerow(("x", "y", "z", "layer", "time"))
    writer.writerows(points)
```

Si ejecutamos el controlador y luego abrimos el archivo generado con una herramienta para manipular hojas de cálculo, deberíamos ver información similar a la siguiente.

![](03_LIDAR/imgs/image-3.png)

A partir de ahora, podemos usar este archivo para analizar en detalle lo que observa el sensor.

[Descargar ejemplo 3 completo](03_LIDAR/ejemplo_3.py)

---
## Ejercicios

Para cada ejercicio se pide armar un programa controlador distinto y entregar los archivos de código.

1. Extraer y visualizar sólo la capa 1 de la imagen de profundidad
2. Exportar en archivo csv sólo la capa 1 de la nube de puntos

# Detección de obstáculos

Ahora que ya sabemos usar algunos sensores avanzados del robot es un buen momento para empezar a pensar en la detección de los obstáculos que puede haber en el mapa.

Recordemos que los obstáculos tienen las siguientes características:

1. Pueden tener cualquier forma, incluyendo rectangular, piramidal, esférica, o cilíndrica.
2. Pueden estar fijados al piso.
3. El color no está especificado.
4. Deben estar al menos a 8 cm de cualquier pared.

![](04_Obstaculos/imgs/image-1.png)

Sabiendo esto podemos imaginarnos múltiples opciones para detectarlos. Por un lado, podríamos usar múltiples sensores de distancia ubicados estratégicamente al frente del robot y tratar de determinar la presencia de un obstáculo mediante los valores que los sensores indiquen. Otra opción posible sería analizar la información provista por el LIDAR. En este ejemplo, iremos por la segunda alternativa. 

Es importante recordar que el LIDAR no es un sensor que se incluya en el robot por defecto, por lo que debemos agregarlo a nuestro robot haciendo una estructura personalizada. Y la ubicación y orientación del sensor son determinantes para la efectividad del programa. En este ejemplo usaremos la misma estructura de robot utilizada para los ejemplos de uso del LIDAR: [robot_lidar.json](04_Obstaculos/robot_lidar.json)

Asimismo, para hacer las pruebas necesitamos un mapa que incluya obstáculos por lo que utilizaremos el mundo [mapa_obstacles_1.wbt](04_Obstaculos/mapa_obstacles_1.wbt), que armamos especialmente para este propósito.

![](04_Obstaculos/imgs/image-2.png)

Dada la información que nos proporciona el LIDAR tenemos también múltiples posibilidades para la detección de obstáculos, algunas más sofisticadas que otras. Vamos a comenzar por la más sencilla: chequear un conjunto pequeño de puntos buscando un patrón determinado que nos indique la cercanía de un obstáculo.

La primera pregunta que nos hacemos es qué puntos verificar. Dependiendo de esta decisión el resultado será distinto, por lo que deberemos evaluar con cuidado para tratar de elegir la opción que mejor permita distinguir los obstáculos de las paredes. En este ejemplo hemos decidido chequear los siguientes puntos:

![](04_Obstaculos/imgs/image-3.png)

La siguiente cuestión a determinar es qué capas observar de la imagen de profundidad que nos devuelve el LIDAR. En nuestras pruebas hemos observado que la capa 0, al estar ubicada encima del robot, suele ser menos precisa que la capa 1. Por esta razón, decidimos utilizar la capa 1, que consideramos que incluye la información de mayor utilidad.

Vamos a comenzar por declarar una función que realizará todo el trabajo de la detección. Llamaremos a esta función “hasObstacle” y la misma servirá para indicar si la información del LIDAR incluye o no un obstáculo. Para ello, lo primero que haremos será calcular dónde se ubican en la lista de valores de profundidad que recibimos como parámetro. Esta lista representa la información de una sola capa del LIDAR (¡importante!) y está compuesta por todos los valores que se obtienen como resultado del escaneo del láser. Es importante recordar que, en el robot que utilizamos para las pruebas, el barrido comienza detrás del robot y continua en sentido horario, generando exactamente 512 valores a lo largo de la circunferencia completa del robot. Sabiendo todo esto podemos calcular fácilmente los índices donde se ubican los puntos que nos interesan.

```python
# Función que busca obstáculos en una capa del LIDAR
def hasObstacle(layer):
    # El LIDAR emite 512 rayos alrededor del robot, pero sólo nos interesan 
    # 5 puntos al frente del robot. Acá calculamos los índices de cada rayo 
    d = math.pi/16
    i0 = int((math.pi - d*4) / math.tau * 512)
    i1 = int((math.pi - d*1) / math.tau * 512)
    i2 = int((math.pi + d*0) / math.tau * 512)
    i3 = int((math.pi + d*1) / math.tau * 512)
    i4 = int((math.pi + d*4) / math.tau * 512)

    print(f"0: {layer[i0]:.3f}, 1: {layer[i1]:.3f}, 2: {layer[i2]:.3f}, 3: {layer[i3]:.3f}, 4: {layer[i4]:.3f}")

    # Umbrales para objetos cercanos y lejanos
    t_near = 0.06
    t_far = 0.2

    # Finalmente chequeamos que los rayos centrales (i1, i2, e i3) detecten
    # objeto cercano pero los extremos (i0 e i4) no detecten nada
    return (layer[i1] < t_near or layer[i2] < t_near or layer[i3] < t_near) \
        and (layer[i0] > t_far and layer[i4] > t_far)
```

Finalmente, en el loop principal, debemos obtener la imagen de profundidad que genera el LIDAR, extraer la capa 1 y luego llamar a la función “hasObstacle” pasando esta información como parámetro. Si la función devuelve True, entonces eso significa que el robot está frente a un obstáculo, en cuyo caso mostramos un mensaje en la consola.

```python
while robot.step(TIME_STEP) != -1:
    # Obtenemos la imagen de profundidad
    image = lidar.getRangeImage()

    # Extraemos de la imagen sólo los datos correspondientes a la capa 1
    layer_1 = image[512:1024]

    # Si detectamos un obstáculo mostramos un mensaje
    if hasObstacle(layer_1):
        print("OBSTÁCULO!")
    else:
        print("NO HAY OBSTÁCULO")
```

Dado que no hemos escrito código para mover el robot, para verificar el correcto funcionamiento del controlador deberemos mover manualmente el robot y ubicarlo en diferentes lugares del mapa. Cuando el robot se encuentre con un obstáculo adelante debería mostrar el mensaje “OBSTÁCULO!” en la consola.

![](04_Obstaculos/imgs/image-4.png)

[Descargar ejemplo completo](04_Obstaculos/ejemplo_1.py)

---
## Ejercicios

Para cada ejercicio se pide armar un programa controlador distinto y entregar los archivos de código.

1. Hacer un controlador que detecte obstáculos usando sensores de distancia
2. Hacer un controlador que pueda explorar el mapa [mapa_obstacles_1.wbt](04_Obstaculos/mapa_obstacles_1.wbt) sin chocar con obstáculos.



# Unidad 4: Sensado Avanzado

En esta unidad aprenderemos a utilizar algunos sensores avanzados que posee el robot, los cuales nos permitirán mejorar nuestra estrategia de navegación en el mapa.

Comenzaremos por los 4 sensores que nos ofrecen información del [posicionamiento del robot](01_Posicionamiento.md): el acelerómetro, el giroscopio, el inertial unit, y el GPS. Saber dónde está ubicado el robot y en qué dirección está mirando nos permitirá movernos en el mapa de una manera más efectiva que lo que nos permitían los [sensores de distancia](../03/04_Distancia.md).

Una vez que podamos obtener la información precisa de la ubicación y orientación del robot, pasaremos a mejorar la navegación de mapas más complejos que puedan incluir trampas como agujeros y pantanos. Para poder detectar estas trampas así como otros tipos de baldosas especiales aprenderemos a usar el [sensor de color de piso](02_ColorPiso.md).

Los agujeros y los pantanos no son las únicas trampas y complicaciones que pueden incluir los mapas más avanzados. El sensor que mejor nos permite entender qué objetos rodean al robot es el [LIDAR](03_LIDAR.md). Veremos entonces qué información nos brinda el LIDAR y cómo usarla para analizar el entorno del robot y detectar así [obstáculos](04_Obstaculos.md) que puedan haber en el camino.

Finalmente, luego de haber aprendido a utilizar todos estos sensores avanzados comenzaremos a desarrollar un algoritmo de mapeo que permita reconstruir en la memoria el camino recorrido por el robot.

[1. Posicionamiento del robot](01_Posicionamiento.md)

[2. Detección del color del piso](02_ColorPiso.md)

[3. Uso del LIDAR](03_LIDAR.md)

[4. Detección de obstáculos](04_Obstaculos.md)

# 1. Instalación de OpenCV y conceptos básicos de imágenes

## Instalación de OpenCV

OpenCV es la librería más popular de procesamiento de imágenes. Es libre, abierta y gratuita, y tiene por detrás una inmensa comunidad que actualiza constantemente con los últimos avances algorítmicos que desarrolla la academia. A pesar de que muchos de los conceptos que presenta no son sencillos (deberíamos realizar un curso específico de procesamiento de imágenes), lo que vamos a necesitar para Rescue es recortado y fácil de comprender. Sin embargo, es una buena oportunidad para introducir a los estudiantes en una rama de la tecnología que está en auge. Muchísimos de los procedimientos de aprendizaje automático basado en imágenes utiliza OpenCV como herramienta de preprocesamiento antes de introducir los datos en redes neuronales. Por lo tanto, como dijimos al principio del curso, Rescue es una maravillosa puerta de entrada para tecnologías de última generación.

Para poder utilizar OpenCV, debemos instalar una librería en Python. Para ello, abrimos una ventana de símbolo de comando y ejecutamos

```bash
pip install opencv-python
```

como vemos en la imagen

![](01_Instalacion/imgs/img1.png)

Si no estaba instalada previamente, sumará a este proceso otra librería excelente llamada numpy.

![](01_Instalacion/imgs/img2.png)

Para comenzar a trabajar con los ejemplos de OpenCV, les solicitamos que bajen el siguiente archivo y lo descompriman en la carpeta que deseen. 

[EjerciciosCamara.zip](01_Instalacion/EjerciciosCamara.zip)

Una vez hecho esto, con la opción de VSC File/Open Folder… abrimos la carpeta Ejercicios Camara. Allí se encontrarán con un conjunto de archivos .py y carpetas con todo el material que utilizaremos en esa unidad.

Para probar que la instalación de OpenCV es correcta, vamos a abrir el programa [testOpencv.py](01_Instalacion/testOpencv.py).

```python	
import cv2

vic=cv2.imread('imagenes\\victimas.png') # lee la imagen de la carpeta imagenes
car=cv2.imread('imagenes\\carteles.png') # Idem

cv2.imshow('victimas',vic) # muestra la imagen vic en una ventana de nombre victimas
cv2.imshow('carteles',car) # muestra la imnagen car en una ventana de nombre carteles
cv2.waitKey(0) # espera la pulsación de una tecla para finalizar la ejecución del programa
```
Y lo ejecutamos con F5. Si todo está instalado correctamente, deben aparecer dos ventanas con imágenes de víctimas y carteles de Rescue, esperando la presión de una tecla para finalizar.

![](01_Instalacion/imgs/img4.png)

Si logran obtener estas ventanas (puede ser que aparezcan apiladas, las pueden mover como cualquier ventana), ¡ya tienen openCV funcionando!

## Conceptos básicos de imágenes

Una imagen no es más que un rectángulo de píxeles. Y cada pixel es un número que representa su aspecto. Este número va a depender del modelo de color utilizado para la imagen. Hay muchos modelos, pero los más populares son RGB (red, green, blue), RGBA (idem pero con un canal de transparencia, alfa), Grayscale (escala de grises) y HSV (hue, saturation, value - matiz, saturación, valor de brillo, también conocido como HSB).

Por ejemplo, en una imagen en RGB, cada píxel es una combinación de 3 bytes, donde cada byte representa un valor entre 0 y 255 para cada color. En RGBA tengo 4 bytes, ya que el cuarto representa la transparencia también con un valor entre 0 y 255. En el caso de escala de grises, cada píxel está representado por un valor entre 0 y 255.

Para ver estas representaciones, ejecutemos el programa [conceptoImagen.py](01_Instalacion\conceptoImagen.py). Podemos ver allí la salida de 4 conversiones distintas:

![](01_Instalacion/imgs/img5.png)

Cuando imprimimos su formato (shape) obtenemos en la ventana de la consola el alto, ancho y la cantidad de bytes por píxel.

![](01_Instalacion/imgs/img6.png)

llí podemos ver que en RGB y HSB tenemos 3 bytes por píxel. En el caso de escala de grises, no está indicado porque es sólo un byte por píxel. Y en RGBA, se suma un cuarto byte para la transparencia.


Y eso es todo: ¡una imagen no es más que un rectángulo de píxeles! Por lo tanto, podemos jugar con la imagen moviendo los píxeles de un lado para otro, cambiando el valor de algunos de sus bytes, etc. Para ver algunas de esas transformaciones, los invitamos a ejecutar el programa [ejemplosBasicos.py]((01_Instalacion\ejemplosBasicos.py)).

# 2. Activación de cámaras del robot y captura de imágenes

Es hora de ponernos a analizar las cámaras de nuestro robot. En primer lugar, el robot por defecto tiene las cámaras ubicadas de una forma no demasiado útil en nuestro caso:

![Imagen 1](02_Captura/imgs/img01.png)

camera_right y camera_left están apuntando en diagonal, y eso no nos va a resultar útil para ver los carteles que queden a nuestra izquierda o derecha. Por lo tanto, vamos a diseñar un robot nuevo (el que está como [robotParaEjerciciosCamaraIU.json](02_Captura\robotParaEjerciciosCamaraIU.json)) que tiene las cámaras al costado y apuntando en forma recta para cada lado:

![Imagen 2](02_Captura/imgs/img02.png)

En este caso las cámaras se llamaran camaraIzquierda (resolución 40x40), camaraFrente (resolución 128x128) y camaraDerecha (resolución 40x40).

Las ruedas son ruedaI y ruedaD.

También tenemos sensores de distancia para los costados y el frente:

![Imagen 3](02_Captura/imgs/img03.png)

Como podemos ver, se llaman distanciaIzquierda, distanciaFrente y distanciaDerecha.


A partir de ahora, vamos a utilizar el mundo **Camara.wbt**. Lo vamos a encontrar en la carpeta World de nuestro set de archivos. Recordar copiar el mundo dentro de game/worlds para que podamos abrirlo.

Ahora vamos a implementar un recorrido siguiendo la pared izquierda, pero con movimientos por baldosa. Podemos verlo en el archivo ContRecorrePorBaldosaIU.py. Si ejecutamos este controlador, podemos ver arriba del mundo, a la izquierda, un cuadro con lo que ve una de las cámaras (la imagen más grande es la de la cámara que está de frente).

![Imagen 4](02_Captura/imgs/img04.png)

Las demás cámaras también tienen su ventanita, más pequeñas. Si hacemos click y arrastramos cada ventana, podremos ver todas al mismo tiempo (En nuestro caso vamos a ver las ventanas de cámara izquierda y derecha más pequeñas).

![Imagen 5](02_Captura/imgs/img05.png)

Ahora bien, ponernos a realizar el procesamiento de imágen para detectar víctimas y carteles directamente sobre el simulador puede ser muy engorroso. Tenemos que cargar el controlador y el robot cada vez, no tenemos forma de ver claramente los procesos intermedios de la imagen, y otras dificultades. **Es por eso que vamos a utilizar nuestro programa para grabar las imágenes de las cámaras y luego procesarlas directamente desde VSC.** Una vez que desarrollemos la función que nos devuelve el tipo de cartel detectado, la incorporaremos en el código de nuestro robot.

Para obtener la imagen de la cámara, utilizaremos el método ***getImage()*** de cada una de ellas. El problema es que el formato que nos devuelve no sirve para grabarlo con openCV. Por lo tanto, debemos convertirlo a un array de numpy, de la siguiente manera:

```python
def convertirCamara(imagen, alto, ancho): #Convierte la imagen de la camara a una imagen de opencv
    return np.array(np.frombuffer(imagen, np.uint8).reshape((alto,ancho, 4)))
```

En esta función, pasamos la imagen capturada por ***getImage()*** y la devolvemos en el formato correcto para grabarla. Luego simplemente utilizamos el método de openCV ***cv2.imwrite(nombreDeArchivo, imagen)*** para almacenarlo.

```python
    cv2.imwrite(f"CI{str(nroImagen).rjust(4,'0')}.png",convertirCamara(camI.getImage(), 40,40))
    cv2.imwrite(f"CD{str(nroImagen).rjust(4,'0')}.png",convertirCamara(camD.getImage(),40,40))
    cv2.imwrite(f"CF{str(nroImagen).rjust(4,'0')}.png",convertirCamara(camF.getImage(),128,128))
```
Dado que el código cuando se carga se ejecuta desde la carpeta *game/controllers/robot0controller* dentro de nuestra instalación de Erebus, tenemos que buscar los archivos grabados de las cámaras allí.

Para poder grabar una secuencia de archivos, utilizamos una variable *nroImagen* que se incrementa, para poder tener archivos distintos. El método ***rjust(cantidad, caracter)*** simplemente agrega el caracter indicado hasta llevar la cadena a la longitud definida en cantidad.

Si ponemos a ejecutar el código [ContRecorreGrabandoIU.py](02_Captura\ContRecorreGrabandoIU.py) hasta recorrer todo el laberinto, nos encontraremos con muchos archivos de las cámaras en las carpetas indicadas. 

![Imagen 6](02_Captura/imgs/img06.png)

***Recomendamos mover los archivos de esa carpeta una vez capturados, porque si ejecutamos otra vez el controlador, va a grabar pisando los archivos anteriormente creados.***



# 3. Detección e identificación de víctimas - Detectando el cartel

Dentro de *capturasSinOrdenar* en la carpeta *Ejercicios Camara*, podemos encontrar un conjunto de capturas ya almacenadas utilizando el algoritmo *ContRecorreGrabando.py*. Es fundamental que clasifiquen esa carpeta de la siguiente manera:

- Poner en una carpeta las imágenes que no tienen carteles.
- Poner en otra carpeta las imágenes que tienen carteles de víctimas.
- Poner en una última carpeta las imágenes que tienen carteles de peligro.

Como son muchas imágenes, pueden hacerse un subconjunto en una nueva carpeta de testeo, donde pongan un puñado de cada una, para ver cómo se comporta el procesamiento final.

Cuando veamos cómo crear mundos, está bueno que se armen los propios, con diferentes carteles, y que utilicen el algoritmo de recorrido para ir grabando otras imágenes. Lo mejor para testear es tener muchos casos distintos.

**TODO LO QUE PRESENTAREMOS DE AQUÍ EN ADELANTE, SON SÓLO SUGERENCIAS. Seguramente hay mejores formas de resolver la detección, y los alentamos a que, junto con sus estudiantes, prueben con diversos recursos de opencv que lo harán más rápido y con más precisión.**

El código que desarrollaremos a continuación está implementado en **deteccionVic01.py**.

Las imágenes que grabamos con la camara de frente son de 128x128, donde cada pixel combina los canales RGBA. 

![](03_CartelV/imgs/img01.png)

En el caso de las víctimas el color no nos interesa, dado que siempre son letras negras (H, S o U) sobre un cartel blanco. Por lo tanto, podemos convertir esa imagen a un modelo de escala de grises, para acentuar las diferencias y trabajar con menos datos. Para ello tenemos que utilizar el método *cvtColor* de opencv.

```python
gris=cv2.cvtColor(imagen, cv2.COLOR_BGR2GRAY)
```
Donde pasamos como primer parámetro la imagen a convertir, y en el segundo caso el modelo de color al cual queremos llevarla. Este método nos devuelve la imagen en ese modelo. Por lo tanto, en *gris* tenemos *imagen*, pero convertida a escala de grises.

![](03_CartelV/imgs/img02.png)

Aún convertida a grises, tengo un degradé bastante amplio (256 valores). Si quiero binarizar la imagen, es decir, convertirla a dos valores, uno para oscuro y otro para claro, podemos ejecutar el método *threshold* de opencv. Para ello, debo definir un umbral. Aquellos pixeles que están por debajo de ese umbral, se convertirán en 0 (negro), y aquellos que no, pasarán a tener un valor determinado que paso como parámetro. La sintaxis es así:

```python
ret, thresh=cv2.threshold(gris, 140, 255, cv2.THRESH_BINARY)
```

En el primer parámetro indicamos la imagen a convertir. En el segundo, el valor del umbral. El tercero es el valor al cual quiero llevar los pixeles que superen los 140. Y el último me indica el tipo de conversión. Como dijimos anteriormente, nuestra conversión es binaria.

Hay diferentes formas de realizar el threshold. Por ejemplo, para manejar diferencias de luz en la imagen, podríamos tener un umbral dinámico según el contexto de los pixeles. Nosotros no tenemos ese problema, con lo cual usamos un mecanismo de umbral simple. Es decir, para cada pixel se aplica el mismo umbral. 

![](03_CartelV/imgs/img03.png)

Podemos ver como la imagen quedó completamente clara. El objetivo ahora es poder recortar el cartel, dejando de lado lo demás. De esta manera podremos detectar en forma sencilla la letra que representa. Para ello, vamos a pedirle a nuestro buen amigo opencv que detecte los contornos con la función *findContours*:

```python	
contornos, jerarquia = cv2.findContours(thresh, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
```

Me devuelve dos valores: en primer lugar, una lista de contornos, donde cada contorno a su vez es una lista de puntitos. En segundo lugar obtenemos un árbol de jerarquías, donde podemos saber qué contorno se encuentra dentro de cuál otro, o cuáles están al mismo nivel. Como no lo usaremos, no nos detendremos en analizarlo.

Con respecto a los parámetros, en el primer caso indicamos la imagen a procesar. El segundo parámetro indica cómo se elabora la jerarquía de contornos (árbol, todos iguales, sólo dos niveles, etc). En nuestro caso, nos interesan sólo los contornos externos, dado que queremos reconocer el cartel. El último parámetro nos permite definir el grado de aproximación al contorno: ¿guardo todos los puntos o tomo sólo los más representativos? Aquí hemos elegido la segunda opción. De todos modos, invitamos a seguir investigando en el siguiente link: [Contornos y cómo dibujarlos en OpenCV](https://omes-va.com/contornos/).

Si una vez detectados, queremos dibujarlos sobre una imagen, utilizamos el siguiente método de openCV:

```python
cv2.drawContours(imagen, contornos, -1, (0, 0, 255), 2)
```

En el primer parámetro indico sobre qué imagen se dibujarán. El segundo define la variable que tiene la lista de los contornos a dibujar. En el tercero puede indicar qué contorno dibujar. Si pongo -1, expreso que quiero todos. El cuarto parámetro es una tupla de 3 valores con la cantidad de azul, de verde y de rojo que tendrá mi línea. Y por último indico el grosor de la línea en pixeles.

![](03_CartelV/imgs/img04.png)

¡Ya lo tenemos en nuestras manos! ¡Traigan la tijera!

## ¡Ejercicio!

Ejercicio 1: hacer una función que dada una imagen me devuelva los contornos si considera que es un cartel, o None si considera que no.










# 4. Detección e identificación de víctima - Identificando la letra

**El código que desarrollaremos a continuación está implementado en deteccionVic02.py**

 A esta altura ya hemos logrado esto:

![](04_Letra/imgs/img01.png)

Ahora queremos recortar el cartel de allí, para tenerlo aislado de lo demás, y luego analizarlo para identificar la letra correspondiente.

Antes que nada, ¿cómo hago en este punto para descartar imágenes que no tengan víctimas, que las tengan a medias, o haya otros elementos en el cartel?

En primer lugar, si el cartel está “limpio” como la imagen anterior, esperamos que el método *findcontours* (usando el parámetro que indica que sólo quiero los contornos externos) detecte sólo uno. Eso lo podemos verificar simplemente con *len(contornos)*.

Pero además, ese único contorno debe tener pocos puntos para ser un rectángulo. En el siguiente ejemplo tenemos un único contorno, pero la cantidad de puntos que lo forman es grande:

![](04_Letra/imgs/img02.png)

Para poder saber la cantidad de puntos de nuestro único contorno, basta con hacer *len(contornos[0])*.

En síntesis, si los contornos detectados pasan ambas condiciones (es uno sólo, y tiene pocos puntos) lo recortaremos y analizaremos la letra.

Para obtener el rectángulo donde está el cartel, vamos a usar el siguiente método:

```python
    approx=cv2.minAreaRect(contornos[0])
```

**minAreaRect** nos devuelve el rectángulo más chico que tenga dentro el contorno pasado como parámetro. De esta manera, aunque el contorno tenga varios puntos, obtenemos un rectángulo preciso. El método nos devuelve la siguiente tupla:


**( (x del centro, y del centro), (ancho, alto), ángulo del rectángulo)**


Por lo tanto, si el ángulo que obtuvimos es múltiplo de 90, vamos a recortar el cartel de la siguiente manera:

```python
        x=int(approx[0][0])
        y=int(approx[0][1])
        mitadAncho=int(approx[1][0]/2)
        mitadAlto=int(approx[1][1]/2)

        rect=thresh[y-mitadAlto:y+mitadAlto, x-mitadAncho:x+mitadAncho] 
```
Finalmente, en *rect* tendremos el cartel limpio, sin nada de lo que está a su alrededor.

*NOTA: Recordemos que vamos a tener que enviar el mensaje sólo cuando estemos suficientemente cerca del cartel. Para ello, una vez que lo recortamos, podemos medir su tamaño y de esa manera decidir si seguimos con la detección de la letra o directamente abortamos el proceso dado que estamos lejos (si se ve chico, es que estamos lejos)*


¿Qué nos queda hacer ahora? ¡Identificar la letra!

Podríamos utilizar algún sistema de reconocimiento de caracteres, pero son lentos, y están destinados a reconocer cualquier caracter de cualquier fuente. Por lo tanto, es una herramienta demasiado poderosa y costosa en tiempo de ejecución para lo que precisamos. Entonces, ¿de qué manera más sencilla podemos reconocer la letra?

De acá en adelante, los invitamos a experimentar. Tal vez una forma sería verificar la cantidad de puntos negros que tiene el cartel. ¿Existe una proporción distinta de puntos si es una S, una H o una U? Para obtener el tamaño del cartel, podemos usar la propiedad shape de la matriz:

```python
        tamanio=rect.shape[0]*rect.shape[1]
```

Luego, para contar los puntos negros, podemos usar el método count_nonzero. Este método cuenta los valores de la matriz que cumplan con la condición que paso de parámetro. En nuestro caso:

```python
        pixelesNegros=np.count_nonzero(rect==0)
```

Si luego calculamos *pixelesNegros/tamanio* podemos obtener el porcentaje de puntos negros con respecto al total del cartel. Pero, ¿esto distingue a las letras? Para saberlo, deberán probarlo con muchas S, H y U, e ir tomando nota de los valores que devuelve, para determinar si encuentran un patrón.

Otra forma sería buscar algunas partes del cartel que puedas ser más significativas según la letra que contengan. Por ejemplo:

![](04_Letra/imgs/img03.png)

Como podemos ver, si tomamos sólo esas dos partes del cartel, en el caso de la H, tenemos 0 puntos negros en ambas; en la S tenemos puntos negros en las dos; y en el caso de la U, sólo tenemos en el cuadro de abajo.

Para obtener los cuadritos, simplemente hacemos:

```python
        cuadritoArriba=thresh[y-mitadAlto:y-int(mitadAlto/3), x-int(mitadAncho/3):x+int(mitadAncho/3)]
        cuadritoAbajo=thresh[y+int(mitadAlto/3):y+mitadAlto, x-int(mitadAncho/3):x+int(mitadAncho/3)]
```

¡Y sólo nos queda contar los puntos negros en cada uno de ellos para saber qué letra es!

¡Recuerden que todo esto debe probarse y ajustarse a partir de muchísimas capturas de víctimas que graben en diferentes laberintos!

## ¡Ejercicios!
Ejercicio 2: hacer una función que dada una imagen, me devuelva el cartel recortado en thresh (blanco y negro) o None si no es un cartel.

Ejercicio 3: armar una función que dada una imagen me devuelva la letra si es un cartel, o None si no lo es.

# 5. Detección e identificación de cartel - Detectando el cartel

**El código que desarrollaremos a continuación está implementado en deteccionCar01.py**

Comencemos ahora con los carteles de advertencia. Desde ya que todo lo que hicimos con las víctimas será de utilidad. Por lo tanto, nuestro primer paso será la extracción del cartel.

![](05_CartelC/imgs/img01.png)

Los primeros pasos serán los mismos: obtener la escala de grises, el threshold, el contorno y el rectángulo donde está inscripto el cartel. 

```python
    gris=cv2.cvtColor(imagen, cv2.COLOR_BGR2GRAY)
    ret, thresh=cv2.threshold(gris, 140, 255, cv2.THRESH_BINARY)
    contornos, hierarchy = cv2.findContours(thresh, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
    approx=cv2.minAreaRect(contornos[0])
    angulo=approx[2]
```

![](05_CartelC/imgs/img02.png)

La única diferencia significativa, es que en este caso los carteles no son un rectángulo con la base paralela al piso, sino que están rotados 45 grados. Por lo tanto, si el ángulo en el que está el cartel es de 45 grados (lo que confirma que es un cartel de advertencia), lo rotaremos para obtener un rectángulo preciso con el contenido del mismo. Con este fin, utilizaremos dos métodos:

1. cv2.getRotationMatrix2D(centro, ángulo, escala): devuelve la matriz de rotación que usaremos más adelante. Debo pasar el punto centro de la imagen, el ángulo que quiero rotar (positivo=a favor de las agujas del reloj), y si quiero hacer una transformación de escala, el valor correspondiente (con 1 mantengo la misma escala)
Con la matriz obtenida ejecutamos el siguiente método:

2. cv2.warpAffine(imagen, matriz de rotación, tamaño de salida): rota la imagen pasada como parámetro, usando la matriz de rotación, y escalando al tamaño de salida indicado.

```python
        alto, ancho=thresh.shape[0], thresh.shape[1]
        M=cv2.getRotationMatrix2D((ancho/2,alto/2),angulo,1)

        thresh_rot=cv2.warpAffine(thresh,M,(ancho,alto))
        imagen_rot=cv2.warpAffine(imagen,M,(ancho,alto))
```

En nuestro caso, aplicamos la rotación a *thresh* (obteniendo *thresh_rot*) para poder buscar el contorno nuevamente; y a *imagen* (obteniendo *imagen_rot*) para poder recortar luego el cartel de allí **porque lo necesitamos en colores para distinguir el tipo de advertencia.**

![](05_CartelC/imgs/img03.png) ![](05_CartelC/imgs/img04.png)

Por lo tanto, a partir de allí volvemos a buscar los contornos, pero ahora sobre thresh_rot y terminamos recortando el cartel.

![](05_CartelC/imgs/img05.png)

## ¡Ejercicios!

Ejercicio 4: realizar una función que dada la imagen de la cámara, me devuelva None si no es un cartel, o el cartel ya rotado y limpio de contexto.



# 6. Detección e identificación de cartel - Identificando el cartel

**El código que desarrollaremos a continuación está implementado en deteccionCar02.py**

Con el cartel limpio, sólo nos falta reconocer el tipo de cartel. Para ello, vamos a hacer algo muy sencillo, dado que la imagen es pequeña. Recorreremos pixel por pixel, obteniendo su valor de azul (b), verde (g) y rojo (r) de la siguiente manera:

```python
b, g, r=rect[x,y]
```
Según los valores que obtengamos allí, iremos contando pixeles negros, blancos, amarillos y rojos. Pero ¿qué valores debe tener para estar dentro de uno de estos cuatro colores? Para saberlo, tenemos que usar alguna herramienta donde podamos estudiar la imagen, de manera tal que nos permita saber el valor de BGR de un pixel determinado de nuestros carteles. Todos los editores de imágenes lo tienen. Mostramos aquí algunos ejemplos de Paint Shop Pro:

![](06_CartelD/imgs/img01.png)![](06_CartelD/imgs/img02.png)![](06_CartelD/imgs/img03.png)

![](06_CartelD/imgs/img04.png)![](06_CartelD/imgs/img05.png)![](06_CartelD/imgs/img06.png)

Es importante que se detengan a probar en muchas imágenes, y en muchos sectores de la imagen. Algo muy importante a tener en cuenta es que los bordes de los colores no son abruptos, con lo cual tenemos zonas donde el valor puede ser algo intermedio.

En nuestro caso, a partir de las muestras que tomamos, definimos estos valores:


Color: B, G, R

Rojo: 79,0, 197

Amarillo: 0, 193, 207

Blanco: 207, 207, 207

Negro: 0, 0, 0 


Con estos valores, y jugando con cierto umbral para no dejar afuera los pixeles de colores intermedios, tenemos:

```python
        amarillo=0
        rojo=0
        negro=0
        blanco=0
        for x in range(rect.shape[0]):
            for y in range(rect.shape[1]):
                b, g, r=rect[x,y]
                if b>150 and g>150 and r>150:
                    blanco+=1
                elif b<50 and g<50 and r<50:
                    negro+=1
                elif b>70 and g<10 and r>180:
                    rojo+=1
                elif b<10 and g>180 and r>190:
                    amarillo+=1
```

Luego, a partir de los valores que obtengamos en estas cuatro variables, podremos definir qué tipo de cartel es cada uno.

## ¡Ejercicios!

Ejercicio 5:

Realizar una función que dada la captura de un cartel, me devuelva None o la letra F (Flammable), P (Poison), C (Corrosive) o O (Organic Peroxide)


**Ejercicio final**

Realizar una función que dada la captura de una imagen, me devuelva None si no es víctima o cartel, o está muy lejos de él, o la letra correspondiente al cartel o víctima.



# 7. Envío de mensajes al supervisor

**El código que desarrollaremos a continuación está implementado en ContEnvioDemensaje.py**

Una vez que mi o mis funciones me devuelven la letra de la víctima o cartel identificado, si el resultado fue positivo, debemos detener nuestro robot, esperar al menos un segundo en ese estado, y luego enviar un mensaje al supervisor. Con tal objetivo, nuestro robot posee un transmisor llamado emmiter que debemos apuntarlo con una variables como a todos los demás dispositivos.

```python
emitter=robot.getDevice("emitter")
```

Cuando queremos enviar el mensaje, el mecanismo es bastante sencillo, como se puede ver a continuación (y es siempre igual, pueden copiar y pegar este código en cualquiera de los robots que desarrollen):

```python
def enviarMensaje(pos1, pos2, letra):
    let=bytes(letra, 'utf-8') # Convertimos la letra a bytes para poder enviarla en la estructura
    # print("Enviando mensaje con posiciones:", pos1, pos2, let)
    mensaje=struct.pack("i i c", pos1, pos2, let) # estructura el mensaje en el formato correcto
    # print(f"Mensaje: {mensaje}")
    emitter.send(mensaje)

def enviarMensajeVoC(tipo):
    parar()
    delay(1200) # Debemos hacer una pausa antes de enviar el mensaje
    enviarMensaje(int(position["x"]*100), int(position["y"]*100), tipo)
```
Para esto creamos dos funciones:

1. enviarMensaje: es de propósito general, dado que podemos enviar otros mensajes además de los reconocimientos de carteles y víctimas, como pedir un LoP (lack of progress), avisar que se llegó al punto inicial y se desea salir, o simplemente avisar que terminó nuestro recorrido. Recibe de parámetro la posición en x, la posición en y la letra que se desea enviar en el mensaje. Dentro de la función se realizan ciertas conversiones con el método *bytes* y con el método *struct.pack* que formatean el mensaje para ser enviado. Finalmente, con el método *send* del emitter logramos la transmisión.

2. enviarMensajeVoC: es la función específica para mandar el mensaje de la víctima o cartel detectado. Simplemente le pasamos la letra que identifica lo reconocido, y automáticamente calcula la posición (hay que enviarla en cm, por eso lo multiplica por 100), y llama a enviarMensaje con los datos necesarios.

En el ejemplo de código veremos que los envíos están harcodeados, es decir, se realiza el envío en un paso determinado del robot. **Está claro que esto sirve sólo para este código y este mapa. El objetivo es armar la función con lo visto en los pasos anteriores, llamarla en cada movimiento de avance o giro mínimo que se realice, y según lo que retorne, llamar a *enviarMensajeVoC* con la letra correspondiente.**

## EJERCICIO FINAL FINAL!!!!

Implementar todo lo visto en el código del robot.


# Unidad 5: Procesamiento de imágenes

¡Y llegó el momento de la magia! En esta unidad nos vamos a adentrar en un tema de muchísima actualidad en el mundo de la computación: el procesamiento de imágenes. En estos últimos años, el aumento de la calidad de las cámaras, junto con su abaratamiento y el crecimiento del poder de cómputo de nuestras computadoras, nos ha puesto al alcance de nuestras manos la posibilidad de reconocer en tiempo real lo que ocurre dentro de un video. No existe área donde no se aplique esta tecnología: reconocimiento de patentes, conteo de personas en un lugar, seguimiento de objetos móviles, detección de enfermedades en el diagnóstico por imágenes, efectos especiales en el cine, reconocimiento de caracteres, y muchísimo más.

Para tener una introducción amable y sencilla comenzaremos con una librería muy conocida en el mundo del procesamiento de imágenes: OpenCV. Es un desarrollo de toda la comunidad científica, donde se implementan los mejores algoritmos que la comunidad ha avalado. Es una librería en constante crecimiento, está portada a los lenguajes más populares de programación, y simplifica muchísimo las tareas más habituales de procesamiento. ¡Bienvenidos al mundo de las imágenes!

[1. Instalación de OpenCV y conceptos básicos de imágenes](01_Instalacion.md)

[2. Activación de las cámaras del robot y captura de imágenes](02_Captura.md)

[3. Detección e identificación de las víctimas - Detectando el cartel](03_CartelV.md)

[4. Detección e identificación de las víctimas - Identificando la letra](04_Letra.md)

[5. Detección e identificación de cartel - Detectando el cartel](05_CartelC.md)

[6. Detección e identificación de cartel - Identificando el cartel](06_CartelD.md)

[7. Envío de mensajes al supervisor](07_Mensajes.md)

