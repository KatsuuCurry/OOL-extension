Jessica Theofilia 894476

L'estenzione "object oriented" di Common Lisp permette l'implementazione di
classi e istanze mediante l'utilizzo delle primitive def-class e make.

IMPLEMENTAZIONE DI UNA CLASSE
(def-class class-name parents part)
Controlla la validità di ogni elemento per la creazione di una classe.
Le informazioni che vengono salvate nella hash-table sono strutturate 
in questo modo: (class-name parents part*) dove part è a sua volta una 
lista di attributi e metodi.

Una volta creata la classe, l'utente potrà controllare l'esistenza della 
classe mediante (is-class class-name).

CONTROLLO DI ATTRIBUTI E METODI
(part-check part)
Si occupa di scorrere tutte le liste di attributi e metodi, controllando 
la struttura di ciascun elemento mediante (field-check) e (method-check).
All'interno di questa funzione viene utilizzato una funzione (applist
function-name list), il quale è una versione di mapcar implementata da 
me che ritorna una lista dei valori ottenuti dall'applicazione della 
funzione function-name passando come parametri gli elementi della lista, 
i quali sono a loro volta delle liste. 
(field-check) e (method-check) controllano la correttezza di ogni 
attributo e metodo.

EREDITARIETÀ
(part-override parent part)
Ritorna una lista con tutte le liste di attributi ricostruiti mediante 
(new-field-list)

(new-field-list parent field)
Controlla tutti i possibili casi di overload di un attributo. In caso 
sia presente un attributo con lo stesso nome nelle superclassi, ne 
confronta eventualmente il tipo, valore o entrambi oppure li eredita. 
La funzione ritorna sempre un attributo costruito in questo modo: 
(nome valore tipo). In caso non abbia valore o tipo, vengono inseriti 
i valori di default (valore = NIL, tipo = T).

(sorted-part parent part)
Ritorna la lista finale degli attributi e metodi da inserire nella 
hash-table. Questa funzione in particolare si occupa di ereditare gli 
attributi e metodi delle superclassi che non vengono ridefiniti dalla 
classe figlio.
È in questo passaggio che viene effettuata la creazione dei metodi.

CREAZIONE DEI METODI
(process-method method-name &rest method-spec)
Si occupa della creazione di funzioni per i metodi, gestendo sia il 
caso in cui il nome del metodo sia una keyword che un simbolo. 
In caso sia una keyword, lo trasforma nell'omonimo simbolo 
(ad esempio :key -> key).
Ritorna il valore di rewrite-method-code, strutturato in questo 
modo: (method-name (lambda))

(rewrite-method-code method-name method-spec)
Si occupa della costruzione del lambda in modo da poterla salvare 
nella hash-table. In questo modo, durante la chiamata del metodo, 
sarà possibile recuperare il corpo del metodo mediante l'utilizzo 
di field. 

CREAZIONE DI UN'ISTANZA
(make class-name &rest field)
Controlla la validità della classe dell'istanza e la correttezza 
dei valori degli attributi.

(instance-field-check class-name field)
Controlla l'esistenza degli attributi dell'istanza nella classe 
e la correttezza dei loro valori in base al tipo indicato.

Una volta creata l'istanza, l'utente potrà controllare la 
correttezza dell'istanza mediante (is-instance value &optional 
class-name).

RECUPERARE I VALORI DI UN'ISTANZA
(field instance field-name)
Ritorna il valore di un'attributo o il corpo di un metodo. 
In caso l'attributo non sia presente nell'istanza, lo cerca nella classe. 
Se non lo trova, lancia un errore.

(field* instance field-name)
Ritorna il valore di un'attributo o il corpo di un metodo percorrendo 
una catena di attributi. Se non lo trova, slancia un errore.

FUNZIONI UTILI
(name-check name) controlla che name sia un simbolo, ma non sia 
una lista vuota.
(get-parent name) ritorna la lista delle superclassi di name.
(get-field-part name) ritorna la lista degli attributi di name.
(get-field name field-name) ritorna il nome, valore e tipo di field-name 
in name.
(get-method-part name) ritorna la lista dei metodi di name.
(get-method name field-name) ritorna il corpo di un metodo field-name 
di name.
(parent-list class) ritorna tutta la lista di superclassi e sè stesso.
(applist function list) applica la funzione function passando come 
parametro gli elementi della lista, i quali sono membri di list 
(list è una lista di liste), e ritorna tutti i valori in una lista.


