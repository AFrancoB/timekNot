# timekNot

Este software es una interfaz textual (un Domain Specific Language o quiza un Esoteric Language) para la creación de relaciones temporales que, por ahora, puede ser instanciado como muestras de audio y/o síntesis de audio como montaje en tiempo real. 

Este lenguaje tiene sus precursores en Nanc-In-A-Can Canon Generator, FluentCan, y timeNot. De igual manera, su desarrollo está profundamente ligado a Estuary como un programa en línea para el live coding multilingüe, colectivo y en red.

Por ahora este lenguaje puede ser utilizado en Estuary en modo Solo. Pero pronto podrá ser utilizado de modo colaborativo. De igual manera, este lenguaje tendrá una página red para analizar y experimentar de manera más didáctica con sus principiops conceptuales más interesantes. Esta pagina tendrá un visualisador de eventos y un dispositivo para desplegar y analizar sus componentes temporales, rítmicos, de afinación justa basados en la investigación de Diego Villaseñor, de sistemas de afinación basados en la investigación de Wendy Carlos y las afinaciones de asia occidental (Irán por ahora pero en un futuro próximo: Palestina y Turquía). 

Este eterno prototipo creció en una cariñosa familia llamada Pirarán, creció en una casita que se llama la Fábrica Colapsada, y fue a la escuelita en McMaster donde, si conoció varixs profesorxs chidillxs, se fue a la huelga con el resto de lxs maestrxs adjuntxs y protestó la presencia de los zionistas en todxs y cada uno de los espacios de esta universidad.

## Cómo habilitar timekNot

### Modo solitario

1. Primero hay que entrar a [estuary.mcmaster.ca](https://estuary.mcmaster.ca)
2. Segundo, para habilitar el modo solo hay que darle click al botón inferior izquierdo grandote que dice "solo mode".
3. Una vez en el "ensamble," recomiendo reconfigurar la vista de Estuary para contar con más espacio en la caja de edición. Dale click al ? en la esquina superior derecha de la página, luego ve a settings y dale scroll hasta llegar al fondo donde verás el View Editor. Copia y pega este código ahí y pucha run: ```grid 2 1 [[label 1,code 2 0 []],[label 3,code 4 0 []]]```
4. Una vez que habilitas una vista más chida, identifica la terminal debajo del área de edición. En este espacio pega el siguiente comando y dale enter: `!exolang "timeknot" "https://afrancob.github.io/timekNot/timekNot.js"`
5. Ok, todo listo para tocar timekNot. En una de las zonas de edicion escribe (hasta arriba) `##timeknot`. Y este es el "hola mundo cruel" para probar que todo está en orden: 

```
a 300cpm | x :| 
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

Esta es la notación de tiempo:

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
Es posible escribir repeticiones para que este bloque de ritmo dure más pero repitiendo un mismo patrón.
```
a 800cpm | !xxoo#4 :| 
a.sound = "drum" .n = 0 3 2 4 5 .pan = 0.3 0.5 0.7;
```
Como seguro han notado ya, la lógica rítmica es aditiva. Es decir al escribir un onset, se crea un evento con una duración determinada, al escribir dos se crean dos eventos cuya duración total es la suma de la duración de dos eventos individuales en lugar de modificar la duración del evento individual. Para tocar con una lógica cíclica y divisiva se deben de usar los [ ].Es posible tener subdivisiones recursivas, potencialmente infinitas:
```
a 800cpm | xxo[o[xx]] :| 
a.sound = "drum" .n = 0 3 2 4 5 .pan = 0.3 0.5 0.7;
```
Esta notación tiene una capacidad para generar patrones euclideanos bastante robusta:
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
La marca de ciclos por minuto es bastante inusual. Es así como he conceptualizado el modo en que Estuary y TidalCycles, al menos MiniTidal, interactúan con marcas de tempo.
Estuary con `!setbpm 120` y Tidal con un programa como este: `s "cp"`.

```
a 1/8 = 120bpm | x :| 
a.sound = "drum" .n = 0 3 2 4 5 .pan = 0.3 0.5 0.7;
```
Esta marca de tempo indica el tempo que una subdivisión genera. `1/1 = 120bpm` es equivalente a `120cpm`. `1/4 = 120bpm` es equivalente al tempo de Estuary. No estoy convencido de que esta sea la mejor ruta para esta indicación de tempo, se revisará para futuras ediciones de TimekNot.

```
a 1.2cps | x :| 
a.sound = "drum" .n = 0 3 2 4 5 .pan = 0.3 0.5 0.7;
```
Ciclos por segundo funciona igual que en TidalCycles. 

```
b 120cpm | x :|
a b 3:2 | x :| 
a.sound = "drum" .n = 0 3 2 4 5 .pan = 0.3 0.5 0.7;
```
La línea a se encuentra a una relación proporcional de 3/2 en relación a b. 

Parser de duración total del bloque rítmico y de aceleración siguen en desarrollo...

#### Notación Politemporal

El aspecto más inmediato de la notación politemporal es similar a la notación de <~ en TidalCycles. Esta permite definir en que punto del bloque de ritmo se alinea con el tempo subyaciente. 

```
a[3] 300cpm | xoxxxoxx :| 
a.sound = "drum" .n = 0 3 2 4 5 .pan = 0.3 0.5 0.7;
```
El tres en corchetes indica que el punto de convergencia entre el tempo y esta línea de tiempo sera el tercer onset rítmico. 

```
a[50%] 300cpm | xoxxxoxx :| 
a.sound = "drum" .n = 0 3 2 4 5 .pan = 0.3 0.5 0.7;
```
El código de arriba alineara el punto de tempo con esta líne temporal elapsada al 50%.

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
a[27] <- b[13] 420cpm | xox[ox]xoxx[oox] :| 

a.sound = "drum" .n = 0 3 2 4 5 .pan = 0.3 0.5 0.7;
b.sound = "glitch" .n = 0 3 2 4 5 .pan = 0.3 0.5 0.7;
```
Aquí se alinea el evento 13 de b con el evento 27 de a. A su vez, el evento 10 de a se alínea con el evento 20 de clock. Si modificamos el tempo de clock notaremos que a y b preservan sus tempos y las proporciones entre ellas pero ambos puntos de alineación cambian. En este caso debemos de imaginar el punto de alineación de a y b lejos como un punto de tiempo arbitrario establecido en



### De la ontología de la diferencia a la expansión canónica

### Expresiones aurales

### Expansión de lo Aural a lo Canónico

### Expresiones de sistemas de alturas

### El pilón: Expresiones de Puntos Temporales

