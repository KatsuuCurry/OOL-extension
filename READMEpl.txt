Jessica Theofilia 894476

L'estenzione "object oriented" di Prolog permette l'implementazione di 
classi e istanze mediante l'utilizzo delle primitive def-class e make.

IMPLEMENTAZIONE DI UNA CLASSE
def_class/2 e def_class/3
Controlla la correttezza dei parametri e memorizza la classe nella base
di conoscenza.

CONTROLLO DI ATTRIBUTI E METODI
parents_check/2 controlla la validità della lista delle superclassi.
check_parts/3, mediante anche l'utilizzo di check_list/3, genera la lista
degli attributi e metodi, escludendo quelli di cui si è fatto un overloading.

L'esistenza di una classe nella base di conoscenza può essere controllata
mediante is_class/1.

EREDITARIETÀ
parents_parts/2 eredita alla classe i metodi e gli attributi delle 
superclassi. 
Effettua un append tra la lista degli attributi e metodi nuovi con la lista
degli attributi e metodi ereditati.
check_new_parts/2 controlla la correttezza dei metodi e attributi. Nel caso
di un metodo, effettua un'asserzione.
field_type_check/3 e type_check/3 ritorna la lista degli attributi ricostruita
dove ogni attributo ridefinito viene confrontato con quello delle superclassi
(subtypep) oppure eredita il tipo. 

CREAZIONE DEI METODI
La creazione di un metodo avviene per asserzione attraverso il predicato 
check_new_parts/2. Tuttavia, prima dell'asserzione, deve essere modificato in
modo tale da poter accettare this come parametro.

param_check/1 si occupa di controllare la correttezza dei parametri del metodo.
substitute_this/3, mediante l'utilizzo di replace_list/4, si occupa di ricreare
tutta la lista delle clause, sostituendo il this con il nome dell'istanza che
chiamata il metodo.

CREAZIONE DI UN'ISTANZA
make/2 e make/3 crea l'utanza di una classe e la memorizza nella base di
conoscenza. 
instance_field/3 controlla che gli attributi dell'istanza esistano nella 
classe.

L'esistenza di un'istanza può essere controllata mediante l'utilizzo di 
is_instance/1 e is_instance/2.

L'istanza o il nome di un'istanza già asserita può essere recuperata mediante 
inst/2.

RECUPERARE I VALORI DI UN'ISTANZA
field/3 recupera il valore di un'attributo o di un metodo. Se non è presente 
nell'istanza, lo cerca nella classe dell'istanza. Se non lo trova, fallisce.

fieldx/3 recupera il valore di un'attributo o di un metodo percorrendo una 
catena di attributi. Se non è presente nell'istanza, lo cerca nella classe 
dell'istanza. Se non lo trova, fallisce.

FUNZIONI UTILI
atomic_check/1 controlla che il parametro sia un atomo, esclusa la lista vuota.
type_of/2 ritorna true se il valore è del tipo indicato.
subtypep/2 ritorna true se il primo tipo è un sottotipo del secondo.
parent_list/2 viene utilizzato per recuperare tutte le liste delle superclassi
di una classe.
defined_inst_check/3 restituisce true se l'istanza nuova è del tipo indicato in 
eventuali attributi presenti nelle classi. Altrimenti restituisce false.