# timekNot

Este software es una interfaz textual (un Domain Specific Language o quiza un Esoteric Language) para la creación de relaciones temporales que, por ahora, puede ser instanciado como muestras de audio y/o síntesis de audio como montajes en tiempo real. 

Este lenguaje tiene sus precursores en Nanc-In-A-Can Canon Generator, FluentCan, y timeNot. De igual manera, su desarrollo está profundamente ligado a Estuary como un programa en línea para el live coding multilingüe, colectivo y en red.

Por ahora este lenguaje puede ser utilizado en Estuary en modo solitario. Pronto podrá ser utilizado de modo colaborativo. También se le encuentra como standalone en mi pagina de github. Este standalone tendrá modos para analizar y experimentar de manera más didáctica con sus principiops conceptuales. Esta pagina tendrá un visualisador de eventos y un dispositivo para desplegar y analizar sus componentes temporales, rítmicos, de afinación justa basados en la investigación de Diego Villaseñor, de sistemas de afinación basados en la investigación de Wendy Carlos y las afinaciones de asia occidental (Irán por ahora pero en un futuro próximo: Palestina y Turquía). 

El standalone está en [https://afrancob.github.io/timekNot/](https://afrancob.github.io/timekNot/)

Este eterno prototipo creció en una cariñosa familia llamada Pirarán, en una obra gris que se llama la Fábrica Colapsada, y fue a la escuelita en McMaster donde, si bien conoció varixs profesorxs y coleguillas chidillxs, se fue a la huelga con el resto de lxs maestrxs adjuntxs y protestó la presencia zionista en los espacios de esta universidad epistemicida.

## Cómo habilitar timekNot en Estuary

### Modo solitario

1. Primero hay que entrar a [estuary.mcmaster.ca](https://estuary.mcmaster.ca)
2. Segundo, para habilitar el modo solo hay que darle click al botón inferior izquierdo grandote que dice "solo mode".
3. Una vez en el "ensamble," recomiendo reconfigurar la vista de Estuary para contar con más espacio en la caja de edición. Dale click al ? en la esquina superior derecha de la página, luego ve a settings y dale scroll hasta llegar al fondo donde verás el View Editor. Copia y pega este código ahí y pucha run: ```grid 2 1 [[label 1,code 2 0 []],[label 3,code 4 0 []]]```
4. Una vez que habilitas una vista más chida, identifica la terminal debajo del área de edición. En este espacio pega el siguiente comando y dale enter: `!exolang "timeknot" "https://afrancob.github.io/timekNot/timekNot.js"`
5. Ok, todo listo para tocar timekNot. En una de las zonas de edicion escribe (hasta arriba) `##timeknot`. Y este es el "hola mundo cruel" para probar que todo está en orden: 

```
a | x :| 
a.s = "cp";
```

### Modo Camarada

Se vienen cosillas chidillas...

### Modo Pedagógico

También acá se vienen cosas...

## Primeros Pasitos

Este es uno de los programas más sencillos en timekNot:
```
a | x :| 
a.sound = "drum" .n = 0 3 2 4 5 .pan = 0.3 0.5 0.7;
```
En la primer línea que ven hay un identificador que puede ser una palabra o una letra. Le sigue una expresión rítmica que termina con una doble barra (sin loop) o una barra de repetición (con loop). En la segunda línea se puede observar como el identificador lleva una cadena de mensajes (sound, n, pan) y cada uno de estos mensajes lleva consigo un string (en caso de sound), una lista de íntegros (en el caso de n) y una lista de números (en el caso del paneo).

Explicaré que se puede hacer con cada una de estas capaz del lenguaje. Empezaré con la notación que, a mi parecer, es el corazón expresivo de este software: la notación temporal.

## Notación de Tiempo

Esta es una expresión de tiempo:

```
a 240cpm | xxox :| 
```
En la línea de arriba se pueden identificar tres partes. De derecha a izquierda: La notación rítmica, la notación de tempo y la notación politemporal. Explicaré una por una, de derecha a izquierda.

#### Notación Rítmica

Todo lo que se escribe entre un pipeline y el signo de doble barra o barra de repetición es la expresión de ritmo de esta capa del programa.

Cada x u o tienen una duración equivalente a un beat o ciclo indicado en la notación de tempo que se explicará a continuación. Por el momento, observemos como el siguiente programa crea dos onsets y dos offsets representados por las x y las o.

```
a 800cpm | xxoo :| 
a.sound = "drum" .n = 0 3 2 4 5 .pan = 0.3 0.5 0.7;
```
Es posible escribir repeticiones para que este bloque de ritmo dure más pero repitiendo un mismo patrón. El patrón de aquí abajo suena igual que el patrón de arriba pero hay otros factores de la escritura que determinan de maneras muy distintas este patrón, especialmente en el modo en el que cada onset activa los samples y sus especificaciones paramétricas.
```
a 800cpm | !xxoo#4 :| 
a.sound = "drum" .n = 0 3 2 4 5 .pan = 0.3 0.5 0.7;
```
Como seguro han notado ya, la lógica rítmica es aditiva. Es decir al escribir un onset, se crea un evento con una duración determinada, al escribir dos se crean dos eventos cuya duración total es la suma de la duración de dos eventos individuales en lugar de modificar la duración del evento individual para ajustarse a un ciclo determinado. Para tocar con una lógica cíclica y divisiva se deben de usar los [ ]. Es posible tener subdivisiones recursivas, potencialmente infinitas:
```
a 800cpm | xxo[xx] :| 
a.sound = "drum" .n = 0 3 2 4 5 .pan = 0.3 0.5 0.7;
```
Esta notación tiene una capacidad para generar patrones euclideanos bastante chingona:
```
a 800cpm | (xx, o[ox], 3, 4) :| 
a.sound = "drum" .n = 0 3 2 4 5 .pan = 0.3 0.5 0.7;
```
```
a 800cpm | (x[ox], 3, 4) :| 
a.sound = "drum" .n = 0 3 2 4 5 .pan = 0.3 0.5 0.7;
```
```
a 800cpm | (3,8) :| 
a.sound = "drum" .n = 0 3 2 4 5 .pan = 0.3 0.5 0.7;
```
```
a 800cpm | _(3,8) :| 
a.sound = "drum" .n = 0 3 2 4 5 .pan = 0.3 0.5 0.7;
```
Como es posible notar, se pueden combinar todas estas funciones:
```
a 400cpm | [(3,8)] [[xx]xo] o[xxx] ![ox]#3 [oxox] ([xx],x,3,7) [!ox#3] :| 
a.sound = "drum" .n = 0 3 2 4 5 .pan = 0.3 0.5 0.7;
```

#### Notación de Tempo
Es posible cambiar el tempo (es decir, la duración de cada x u o) de nuestra expresión temporal. Hay varias opciones:

```
a | x :| 
a.sound = "drum" .n = 0 3 2 4 5;
```
La ausencia de una marca de tempo, como el programa de arriba, indica que se está usando el tempo externo (por ahora, el tempo de Estuary). El beat de Estuary marca 120 beats por cuarto de nota por default. Lo que escuchan con este programa debe de corresponder a un metrónomo marcando 120BPMs. Es posible cambiar el tempo de Estuary al escribir `!setbpm 90` en la terminal. Podrán notar que el tempo de este programa cambia también. 

```
a 120cpm | x :| 
a.sound = "drum" .n = 0 3 2 4 5 .pan = 0.3 0.5 0.7;
```
La marca de ciclos por minuto es bastante extraña. Es así como he conceptualizado el modo en que Estuary y TidalCycles, al menos MiniTidal, interactúan con marcas de tempo.
Estuary con `!setbpm 120` y Tidal con un programa como este: `s "cp"` suenan igual a esto:
```
a 120cpm | x :| 
a.sound = "cp";
```
Inclusive igual a este, dada la lógica aditiva de timekNot:
```
a 120cpm | xxxxxx :| 
a.sound = "cp";
```
Además de politemporalidad y ritmos aditivos, timekNot ha sido concebido desde una lógica multilíngüe y colectiva. Es decir, timekNot debe de tener opciones de sincronización simples con otras lógicas como la de MiniTidal y el tempo subyaciente de Estuary. La ruta para una multilingualidad  sencilla sería colapsar las diferencias entre MiniTidal, Estuary y timekNot en una standardización dominante. La solución que encontré es amplificar las diferencias y abrir vías de alineación sencillas como la marca de ciclos por minuto.

```
a 1/8 = 120bpm | x :| 
a.sound = "drum" .n = 0 3 2 4 5 .pan = 0.3 0.5 0.7;
```
Esta marca de tempo indica el tempo que una subdivisión genera. `1/1 = 120bpm` es equivalente a `120cpm`. `1/4 = 120bpm` es lo que escucharían si activan un metrónomo común a 120bpms. No estoy convencido de que esta sea la mejor ruta para esta indicación de tempo, se revisará para futuras ediciones de timekNot quizá. Por lo pronto, es importante utilizar esta notación en el nivel de recursión rítmica más alto para crear poliritmos, así es posible producir un polirritmo de cuatro contra seis:
```
a 1/4 = 120bpm | xxxx :| 
a.sound = "drum" .n = 0 3 2 4 5 .pan = 0.3 0.5 0.7;
b 1/6 = 120bpm | xxxxxx :| 
b.sound = "808" .n = 0 3 2 4 5 .pan = 0.3 0.5 0.7;
```
Ciclos por segundo funciona igual que en TidalCycles. 
```
a 1.2cps | x :| 
a.sound = "drum" .n = 0 3 2 4 5 .pan = 0.3 0.5 0.7;
```
La línea a se encuentra a una relación proporcional de 3/2 en relación a b. 
```
b 120cpm | x :|
a b 3:2 | x :| 
a.sound = "drum" .n = 0 3 2 4 5 .pan = 0.3 0.5 0.7;
```
Parser de duración total del bloque rítmico y de aceleración siguen en desarrollo...

#### Notación Politemporal

El tempo en Estuary es un objeto que tiene 3 valores. El primero es una frecuencia, es decir el número de beats o ciclos por segundo. El segundo es un punto de tiempo, es decir una fecha y hora (UTC time) que ancla el tempo. Por último un número de beats/ciclos elapsados en el punto de tiempo indicado en el segundo valor. El desarrollo de esta concepción de tiempo se alínea con el desarrollo de la concepción de politemporalidad presente en timekNot, timeNot, y Nanc-In-A-Can Canon generator. Toda línea temporal necesita una frecuencia que determina la duración entre eventos (la marca de tempo en caso de timekNot) y una referencia externa que le ancla a un tiempo inter-subjetivo. Pero también es necesario marcar un punto de la estructura de la línea temporal donde se produce el contacto entre el tiempo externo y esta. Lo que aquí llamo notación politemporal es el modo que he diseñado para indicar estos puntos de contacto, o convergencia. Estas convergencia pueden ser externas, el punto de contacto entre el tempo externo y la línea que se describe, o internas, el punto de contacto entre dos líneas de tiempo descritas en timekNot. 

Cabe mencionar que uno puede escribir referencias recursivas que romperan el programa como este:
```
a <- b 300cpm | xoxxxoxx :| 
b <- a 120cpm | x :|
```
Al menos una de las expresiones de tiempo anteriores necesita una referencia al tempo externo. 

El aspecto más inmediato de la notación politemporal es similar a la notación de <~ en TidalCycles. Esta permite definir en que punto del bloque de ritmo se alinea con el tempo subyaciente. 

```
a[3] 300cpm | xoxxxoxx :| 
a.sound = "drum" .n = 0 3 2 4 5 .pan = 0.3 0.5 0.7;
```
El tres en corchetes indica que el punto de convergencia entre el tempo externo y esta línea de tiempo sera el cuarto onset rítmico. Es decir este: `xoxXxoxx`. La mayúscula en este pseudocódigo indica el punto de este programa donde se alineará con el punto donde el segundo valor del tempo en Estuary. Es importante señalar que estos modos de alineación tienen poco que ver con definir el tempo de la línea de tiempo. El punto más importante de este software es que invita a quien toca a que cada expresión tenga un tempo diferente. Si desean alinear el tempo de su linea temporal con el tempo externo no escriban nada en la notación de tempo. 

```
a[50%] 300cpm | xoxxxoxx :| 
a.sound = "drum" .n = 0 3 2 4 5 .pan = 0.3 0.5 0.7;
```
El código de arriba alineara el punto de tempo con esta línea temporal elapsada al 50%.

```
a[5-0] 300cpm | xoxxxoxx :| 
a.sound = "drum" .n = 0 3 2 4 5 .pan = 0.3 0.5 0.7;
```
Esta notación puede replicar la estructura del bloque rítmico. 5-0 quiere decir que el quinto bloque de ritmo y el evento 0 en dicho bloque serán alineados con el punto de tempo correspondiente.

```
a[5-1.2] 300cpm | x[o[oox]]xxxoxx :| 
a.sound = "drum" .n = 0 3 2 4 5 .pan = 0.3 0.5 0.7;
```
Esta notacion nos permite alinearnos con eventos anidados. Este código está indicando que el punto de alineamiento es la x en el corchete dentro del corchete del ejemplo. 

Es aquí donde la cosa se pone complicada.

```
clock 300cpm | xxxx :| 
a <- clock 500cpm | xox[ox]xoxx[oox] :| 
a.sound = "drum" .n = 0 3 2 4 5 .pan = 0.3 0.5 0.7;
```
En este ejemplo a no esta alineada con el tempo externo, sino con la línea clock. Es decir, el momento 0 de clock y el momento 0 de a convergen. En este ejemplo esto es insustancial porque de manera indirecta a y clock estan alineados con el tempo externo. Pero en timekNot esto se puede modificar de maneras sustantivas.

```
clock 300cpm | xxxx :| 
a[10] <- clock[20] 500cpm | xox[ox]xoxx[oox] :| 
a.sound = "drum" .n = 0 3 2 4 5 .pan = 0.3 0.5 0.7;
```
En el ejemplo anterior, se alinea el evento 10 de a con el evento 20 de clock. 

```
clock 300cpm | xxxx :| 
a[10] <- clock[20] 500cpm | xox[ox]xoxx[oox] :| 
b[27] <- a[13] 420cpm | xox[ox]xoxx[oox] :| 

a.sound = "drum" .n = 0 3 2 4 5 .pan = 0.3 0.5 0.7;
b.sound = "glitch" .n = 0 3 2 4 5 .pan = 0.3 0.5 0.7;
```
Aquí se alinea el evento 13 de b con el evento 27 de a. A su vez, el evento 10 de a se alínea con el evento 20 de clock. Si modificamos el tempo de clock notaremos que a y b preservan sus tempos y las proporciones entre ellas pero ambos puntos de alineación cambian. En este caso debemos de imaginar el punto de alineación de a y b lejos como un punto de tiempo arbitrario que solo de maneras indirecta influye en como se alínean los sonidos. Pero, con timekNot es posible incorporar estos puntos de convergencia a nuestra escucha inmediata del programa con una notación adicional donde incorporo el tiempo de evaluación con el tiempo elapsado hasta aquel momento de los beats o ciclos de la línea de tiempo referida. Es decir, podemos decirle a timekNot que queremos escuchar la convergencia de dos líneas temporales después o durante nuestra evaluación. Este programa lo hace posible:
```
clock 300cpm | xxxx :| 
a[10] <- clock[20] 666cpm | xox[ox]xoxx[oox] :| 
b[27] <- a[2>>] 420cpm | xox[ox]xoxx[oox] :| 

a.sound = "drum" .n = 0 3 2 4 5 .pan = 0.3 0.5 0.7;
b.sound = "glitch" .n = 0 3 2 4 5 .pan = 0.3 0.5 0.7;
```
El símbolo `>>` que llamo snapCeil permite indicarnos cuando empezará el bloque siguiente de donde se ha evaluado el programa. Si uno evalúa el programa en el bloque 30, entonces el valor de snapCeil será 31. En timekNot se pueden utilizar dos símbolos similares: `<<` que es snapFloor, es decir el valor es el inicio del bloque donde se ha evaluado el programa  y `<>` que es snapRound que, dependiendo del momento exacto de evaluación actuara como snapFloor o snapCeil. 

En la línea que crea la relación entre b y a lo que se expresa es lo siguiente: Alínea el evento 27 de b con a en dos eventos despues del bloque posterior al bloque cuando se evaluó el programa. Asumamos que uno evalúa durante un bloque que inicia en el evento 150, el siguiente bloque inicia en 158 (cada corchete cuenta como un evento). Entonces, el evento 27 de b convergerá con el evento 161 de a. 

Es posible determinar convergencias usando modulo para determinar cada cuantos bloques rítmicos se debe de esperar para escuchar la convergencia. La notación para esto estará lista pronto...

### De la Ontología de la Diferencia a la Expansión Canónica

## Expresiones Aurales

Esta es una expresión aural:
```
a.sound = :cycleBlock "drum glitch" .n = :cycle 0 3 2 4 5 .pan = :spread 0.3 0.5 0.7 .gain = :cycleTrunc  0.8 0.9 0.8 1.1 1 0.9 0.8 0.9;
```
Es decir, un identificador con una serie de mensajes y cada mensaje un span y una serie de valores. Importante señalar que cualquier expresión aural debe de terminar en punto y coma. 

Estos son los posibles mensajes que se le puede mandar a un identificador:

1. s (también se puede usar sound). Identifica el banco de samples. 
2. n. Identifica el índice del sample que se quiere tocar. 
3. gain. Ganancia normalizada de 0 a 1 (donde 1 es 0db).  
4. pan. Valor de paneo. 
5. speed. Velocidad de reproducción de la muestra.





### Expansión de lo Aural a lo Canónico

## Expresiones de Alturas

## El pilón: Expresiones de Puntos Temporales

## Ejemplos Chidillos pa Explorar

```
##timeknot

clock 500cpm | x :|   

a[2] <- clock[2>>] 300cpm * [1,1.1,1.2,1.3,1.4,1.5] | ox[xx]xo[ox]x :|

a.s = "grandpiano" .speed = 1 .n = _-_ 6 7 8 7 6 10 5 
.pan = 0.5 * [0, 0.5, 0.7, 1.3, 1.5, 2]
.alpha = _-_ 0 2 5 0 1 3 7 8 6 7 + [0,2,4,5,6,7];
```