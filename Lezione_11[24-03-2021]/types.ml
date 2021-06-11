(**

Vogliamo definire un controllore del
tipo per il linguaggio di programmazione
funzionale che abbiamo visto con l'interprete.
Allora la prima cosa
definiamo quali sono i tipi elementari
del programma del linguaggio.
La sintassi del linguaggio
era fatta da costanti
intere, delle costanti booleane,
la possibilità di fare uso di variabili,
il let,
un certo numero di operazioni
primitive, ifthenelse,
il lambda, cioè la possibilità di
definire una funzione che prende
un parametro e ha un corpo,
e poi la call in uno stile funzionale
che prende due espressioni,
una prima espressione,
sarà valutata a una funzione,
il secondo è l'espressione
del parametro attuale.
Allora a questo se dovevamo
eseguire l'interprete,
il passo successivo era definire
quali sono i valori calcolati,
noi non dobbiamo eseguire
in termini di valore,
ma dobbiamo eseguire
l'interprete del linguaggio
per fare il controllo dei tipi in base
a un altra classe di valori che sono
i tipi esprimibili nel linguaggio.
Come sono i tipi esprimibili del
linguaggio? Abbiamo introdotto
il tipo ttype dei tipi che possono
essere i tipi degli interi,
i tipi dei booleani perché abbiamo
dei valori primitivi
che sono interi e booleane.
Ovviamente la struttura dei
tipi primitivi dipenderà dalle
caratteristiche del linguaggio,
un linguaggio che ha
stringhe e altre operazioni complicate
avrà bisogno di rappresentare i
tipi primitivi per tutti i valori
di base del linguaggio.
Le funzioni prendono un
argomento e restituiscono un risultato.
Il tipo di una funzione
la vediamo come una coppia,
il tipo dell'elemento e il
tipo del risultato. Adesso,
andiamo a vedere sinteticamente
come è definita una funzione.
Annoto
la funzione con il tipo dell'argomento
infatti se voi andate a vedere come è
fatto il lambda rispetto alla definizione
che abbiamo dato a quella senza tipi,
il lamba è fatto dal costruttore di
una funzione che mi prende una
stringa che è il parametro, annoto
il parametro con il tipo
dell'argomento e poi
dico com'è il body della funzione,
non dico com'è,
il tipo del risultato.
Il tipo del risultato
lo calcolo però non inferisco
il tipo dell argomento questo
perché per semplicità è molto
più comodo annotare piuttosto che
far Type inference. Questo
già dalla mia osservazione vi
dice che quello che stiamo facendo
stiamo facendo un typechecker.
Non stiamo sviluppando un sistema
di inferenza dei tipi.

Abbiamo bisogno ancora
una volta dell'ambiente,
allora l'ambiente abbiamo detto che
quando andiamo a farle l'esecuzione,
un ambiente è un meccanismo che viene
utilizzato per dare una astrazione
di che cosa c'è nello
stack dei record di attivazione.
Mi permette di dire, data una
variabile, qual è
il valore associato a quella variabile
che è esattamente quello che succede
quando vado a fare accesso allo stack.
Ovviamente nell astrazione del
comportamento dell'ambiente,
non vedo lo stack,
vedo le variabili,
vedo i nomi delle variabili, quando
sarà a run time non vedo il nome delle
variabili perché vedo soltanto la
posizione relativa della variabile in
base all'indirizzo di base dello stack,
perché il meccanismo di accesso al
solito sono indirizzo di base più offset,
quindi il nome della variabile è la
locazione intesa come offset relativo.
Staticamente ho una struttura
che è esattamente l'ambiente,
ma nei linguaggi dei compilatori
viene chiamata la tabella dei simboli.
Che cosa contiene la tabella dei simboli.
La tabella dei simboli contiene
associata ad ogni identificatore
controllato nel programma,
le annotazioni di
informazione statica che posso
associare in quel programma.
Quindi quello che mi interessa
staticamente è vedere un nome e
vedere qual è la notazione associata
a quel nome, il tipo ad esempio.
Allora notate che questo è esattamente
l'ambiente, definito come una lista generica
di coppie stringa per un valore
generico e continua a essere tale.
Avevamo definito l'operazione di
lookup come un operazione che andava
a vedere nell'ambiente qual era
il valore associato alla stringa e
la usiamo tale quale. Quindi questo
cosa vuol dire?
Vuol dire che la struttura di supporto
è parametrica perché è l'astrazione
di poter associare dei valori a dei nomi.
Nel caso di esecuzione,
i valori sono i valori a run time,
nel caso analisi statica i valori sono
le annotazioni di tipo associate
alle variabili.
Vi ricordate che
quando noi abbiamo definito l'interprete,
abbiamo detto dobbiamo avere per
l'operazione primitiva che la macchina
astratta di esecuzione del linguaggio,
deve saper fare
le somme deve saper fare la differenza
e vogliamo utilizzare i meccanismi
primitivi per far somma e differenza.
Abbiamo i tipi di base
per le operazioni primitive,
ad esempio più è un tipo funzionale
che mi prende un intero,
è un qualcosa che
mi fa una somma tra interi e interi,
e mi restituisce un intero e così via.
Vuol dire che nella tabella dei
simboli del linguaggio va ad assumere
subito di avere i valori dei tipi
relativamente all'operazione primitive
del linguaggio.
Quando opero su una macchina
virtuale con un linguaggio tpe safe
so che c'ho il tipo degli
interi e 
non me ne frega niente
di come è rappresentato.
E so anche quali sono le annotazioni
di tipo per le operazioni primitive.
Una volta fatto questo,
eccolo qui, riesco a definirmi
sostanzialmente la componente
del type checker, che ho chiamato
type_of.
Prende un ambiente di tipi,
qindi la tabella dei simboli
che chiamiamo gamma.
Gamma viene perché normalmente
i sistemi di regole hanno le
assunzioni con le gamma scritte e
un'espressione che è il programma
di cui vogliamo
controllare il tipo.
Il programma è rappresentato
dall'albero di sintassi astratta.
Cosa fa l'analizzatore statico?
Fa un'analisi
a partire dall'albero di sintassi astratta
del programma che stiamo considerando,
quindi va a vedere le caratteristiche
del programma e ricorsivamente
determina il tipo del programma
dalla struttura sintattica del programma.
Se il programma è una costante intera,
il risultato sarà il tipo
associato alle costanti intere.
Se il programma è una costante booleana
e notato che non vado a vedere il
valore me ne frega niente come il valore,
so che quella è una costante di
tipo intero e quindi sto facendo un
astrazione e quindi sto facendo un'
approssimazione del comportamento a
tempo di esecuzione perché poi dopo
il comportamento di esecuzione sarà
dipendente dal valore perché poi dovrò
andarlo a vedere a run time valore.
Astrattamente,
quando io vado a vedere solo nelle
annotazioni astratte mi interessa
solo sapere che quello è un intero,
non che  il valore sarà 25,
32 o 44,
il comportamento è indipendente dal
valore rispettivo. Trovo una variabile,
una variabile, io non lo so,
è qualcosa che ha definito il programmatore,
quindi per vedere qual'è il valore
della variabile dico all'oracolo, tabella
dei simboli, qual è l'informazione
che ho messo relativamente a questa
variabile nella tabella dei simboli?
Notate che se l'operazione di lookup mi
dice che non lo trova vuol dire che io
sto usando una variabile che non l'ho
dichiarata prima altrimenti
 il risultato sarà il tipo associato.

A questo punto la scelta che viene
fatta in questo linguaggio di
programmazione è di dire che il tipo
del ramo then ed else deve essere lo stesso.
Se sono diversi devo dare un errore di tipo.
Adesso chiedo a coloro che hanno già visto
un po di sistemi di tipi,
perché questa scelta? Chi mi risponde?
Quando prima ho detto che noi quando
consideriamo il linguaggio di programmazione,
abbiamo un meccanismo di safety
che ho chiamato la safety2,
cioè i comportamenti
sono tutti definiti e vogliamo
operare in composizionalità,
bene questo garantisce
la composizionalità,
perché se ho un'espressione di ifthenelse,
che è usata
in un contesto dove ci sono
altre espressioni attorno,
non posso dargli come tipo un risultato,
una volta che mi dà un risultato perchè sennò
non avrei
un meccanismo di composizionalità.
Questa regola di tipo vi fa
anche vedere un'altra cosa.
Ad esempio, se io scrivessi questo programma:

if true then 15 else true

Questo programma supera
il controllo dei tipi? No a causa della incompatibilità di tipo.
La guardia Tbool,
il ramo then ha type Tint.
Però se voi andate a
vedere questo programma,
questo programma è corretto
dal punto di vista dei tipi?
Potrà mai dare un comportamento
diverso da un espressione intera?
Darà sempre risultato 15.
Dal punto di
vista del tipo questo programma è
un programma corretto.
E' un programma corretto perché 
è esattamente un valore intero e di
fatto equivalente alla costante intera
15 sintatticamente scritta
in un altro modo.
Allora questo è un esempio per farvi
vedere che i sistemi di tipi introducono
un' astrazione di comportamento,
cioè ci sono dei comportamenti che
vengono astratti e che poi dopo potrebbero anche mai
non presentarsi a tempo di esecuzione.
In modo particolare,
questo è un comportamento di un
programma non corretto che però non si
manifesta mai a tempo di esecuzione,
però per evitare danni lo escludo.
Allora questa è la scelta che
viene fatta in linguaggi 
tipo ocamel
proprio per garantire la composizionalità,
per essere sicuro di garantire la
composizionalità e questo semplice
esempio vi fa vedere qual è il tipo
di astrazione che viene fatta.

Nella toolchain dei compilatori
fare il controllo dei tipi
è scrivere sostanzialmente un
interprete del linguaggio dove
l'interprete del linguaggio usa
come valori di base i tipi,
quindi si fa un esecuzione simbolica
rispetto ai tipi. Se l'esecuzione simbolica
rispetto ai tipi non solleva un'eccezione,
vuol dire che il programma
è type safe rispetto ai tipi e
questa esecuzione simbolica viene
fatta staticamente.
Però si perde un pò di tempo,
certamente,
ma quando uno è in fase di analisi statica,
se il compilatore ci mette un secondo in più,
ma se ne strafrega assolutamente.
Magari si prende un caffè in più,
questo è l'unico effetto netto,
però poi l'effetto del
linguaggio è che il controllo dei
tipi è avvenuto prima e quindi
non ho errori di tipo. Va bene?
La cosa molto più complicata
nel caso della Type inference.




**)

type ttype =
	| Tint
	| Tbool
	| Tfun of ttype * ttype;;

type expr = 
	| CstI of int
	| CstB of bool
	| Var of string
	| Let of string * expr * expr
	| Prim of string * expr * expr
	| If of expr * expr * expr
	| Fun of string * ttype * ttype
	| Call of expr * expr;;
	
type 'v env = (string * 'v) list;;

let rec lookup env x =
	match env with
	| [] -> failwith(x ^ " not found")
	| (y,v)::r -> if x=y then v else loookup r x;;

let basicTypes = [
	"+", Tfun(Tint, Tfun(Tint, Tint));
	"-", Tfun(Tint, Tfun(Tint, Tint));
	"*", Tfun(Tint, Tfun(Tint, Tint));
	"<", Tfun(Tint, Tfun(Tint, Tbool));
	"and", Tfun(Tbool, Tfun(Tbool, Tbool));
	"or", Tfun(Tbool, Tfun(Tbool,Tbool))
	];;
	
exception Type_error of string;;

let rec type_of_gamma e =
	match e with
	| CstI(_) -> Tint
	| CstB(_) -> Tbool
	| Var(x) -> lookup gamma x
	| Let(x, e1, e2) ->
		let t1 = type_of gamma r1 in
			type_of ((x,t1)::gamma) e2
	| If(e1,e2,e3) ->
		if (type_of gamma e1) = Tbool then
		let t2 = type_of gamma e2 in
		let t3 = type_of_gamma e3 in
		if t2 = t3 then t2 else raise (Type_error "if branches have different types")
		else
			raise (Type_error "if with no a boolean guard")
	| Fun(x, tx, e) -> Tfun(tx, type_of ((x,tx)::gamma) e)
	| Prim("-",e1,e2) ->
		
