# Interprete de lambda calculo
## Autores

Juan Villaverde Rodríguez - juan.villaverde.rodriguez@udc.es

Carlos Rodriguez Rojo - carlos.rojo@udc.es

## Manual de usuario
### **1.1 Implementacion reconocimiento expresiones multi-linea.**
El interprete reconoce expresiones hasta que encuentra dos puntos y coma seguidos '**;;**'.

### **2.1 Incorporación de un combinador de punto fijo interno.**

Es posible funciones recursivas mediante la palabra clave **letrec**, de forma que en lugar de escribir:
```
let fix = lambda f.(lambda x. f (lambda y. x x y)) (lambda x. f (lambda y. x x y)) in
let sumaux =
lambda f. (lambda n. (lambda m. if (iszero n) then m else succ (f (pred n) m))) in
let sum = fix sumaux in
sum 21 34
```
Puedes escribir:
```
letrec sum : Nat -> Nat -> Nat =
lambda n : Nat. lambda m : Nat. if iszero n then m else succ (sum (pred n) m) in
sum 21 34
```

### **2.2 Incorporacion de un contexto de definiciones globales**

Permite asociar nombres de variables con valores o terminos asi como la creacion de alias de tipos de la misma manera:

```
identificador = termino
x = false;;
N = Nat;;
lambda x: N. x;;
```

### **2.3 Incorporacion del tipo string**

Se incorpora el tipo string de forma que se puedan formalizar cadenas de caracteres asi como la concatenacion de strings y...

Para formalizar un string se hace mediante las dobles comillas de la siguiente manera:

```
"srt";;
"Hola Mundo";;
```

Para la concatenacion se utiliza la palabre clave **concat**:
```
concat "hola " "mundo";;
```
A mayores, se añadio una funcion que permite poner a mayusculas un string, dicha funcion se utiliza con la palabra clave **capitalize**

```
capitalize (concat "hola " "mundo";;)
```

### **2.4 Incorporación del tipo tuplas**

Mediante los corchetes se puede definir el tipo tuplas siendo **{}**
la tupla vacia. Tambien se pueden proyectar sus elementos mediante un punto y un entero empezando desde el 0:
```
{1,"hola mundo", if true then false else true} : {Nat, String, Bool} 
{3,4}.1 --> devolveria 4
```

### **2.5 Incorporación del tipo registros**

Para definir un registro tambien se utilizan los corchetes y una etiqueta asociada a cada termino de la siguiente manera:
```
{hola=2, mundo="srt",adios=true} : {hola:Nat, mundo:String, adios:Bool};;
```

Para su proyeccion se utiliza la etiqueta:

```
{hola=2, mundo="srt"}.hola ---> 2
```

No se contempla el registro vacio, **{}** sigue siendo una tupla vacía.

### **2.7 Incorporacion de variantes**

Para definir las variantes se hace uso de los simbolos de etiqueta: "<>"
Antes de definir un termino, debemos definir un tipo de variantes.


```
Int = <pos:Nat, zero:Bool, neg:Nat>;;
```
Posteriormente, podemos acceder a cada termino dentro de ese tipo de la siguiente manera:

```
p3 = <pos=3> as Int;;
```



### **2.8 Incorporacion de listas**

Tambien se ha incorporado la posibilidad de definir y trabajar con listas
La sintaxis y operaciones para ello es la siguiente:

T => Tipo de dato (Nat, Bool, Nat -> Bool...)
l => Termino de tipo lista
t => Termino

Crear una lista vacia: nil[T]
```
nil[Nat];;
```

Crear una lista con elementos: cons[T] t l
```
cons[Nat] 3 nil[Nat];;
```

Comprobar si una lista esta vacia: isnil[T] l
```
isnil[Nat] (nil[Nat]);;
isnil[Nat] (cons[Nat] 3 nil[Nat]);;
```

Obtener la cabeza de una lista: head[T] l
```
head[Nat] (cons[Nat] 3 nil[Nat]);;
```

Obtener la cola de una lista: tail[T] l
```
tail[Nat] (cons[Nat] 5 (cons[Nat] 3 (cons[Nat] 9 nil[Nat])));;
```
