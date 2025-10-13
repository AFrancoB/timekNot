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