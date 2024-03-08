%%%% Jessica Theofilia 894476

%%%% -*- Mode: Prolog -*-
%%%% oop.pl --

:- dynamic class/3.
:- dynamic instance/3.
:- dynamic instance_in_class/3.

% def_class/2 e def_class/3
% Controlla se i parametri siano del tipo corretto e memorizza
% la classe creata nella base di conoscenza.

def_class(ClassName, Parents) :-
    def_class(ClassName, Parents, []).

def_class(ClassName, Parents, Parts) :-
    atomic_check(ClassName),
    findall(ClassName,
            class(ClassName, _, _),
            []),
    parents_check(Parents),
    sort(Parents, ParentsChecked),
    parents_parts(ParentsChecked, ParentsParts),
    check_new_parts(Parts, ClassName),
    sort(Parts, PartsNoDouble),
    field_type_check(ParentsParts,
                     PartsNoDouble,
                     PartsChecked),
    check_parts(ParentsParts,
                PartsChecked,
                PartsFinal),
    assertz((class(ClassName, ParentsChecked, PartsFinal))),
    format("Class named ~w has been created", [ClassName]),
    !.

def_class(ClassName, _, _) :-
    findall(ClassName,
            class(ClassName, _, _),
            List),
    List \= [],
    format("Error: Class ~a already exists.", [ClassName]),
    !,
    fail.


% atomic_check/1
% Controlla se Term è un simbolo. Viene escluso [].

atomic_check(Term):-
    atomic(Term),
    Term \= [],
    !.

% parents_check/2
% Controlla se Parents è una lista di classi esistenti.

parents_check([]).

parents_check([Parent | T]) :-
    is_class(Parent),
    parents_check(T),
    !.

% parents_parts/2
% Eredita alla classe i metodi e gli attributi delle
% superclassi.

parents_parts([], []).

parents_parts([Parent | T], PartsFinal) :-
    parents_parts(T, ParentsParts),
    findall(P, class(Parent, _, P), [PartsList]),
    append(PartsList, ParentsParts, PartsFinal).


% check_new_parts/2
% Controlla se i metodi e attributi siano dichiarati
% correttamente. Per i metodi, effettua un'asserzione.

check_new_parts([], _):- !.

check_new_parts([Field | T], ClassName):-
    Field =.. [field, FieldName, Instance],
    atomic_check(FieldName),
    is_instance(Instance),
    check_new_parts(T, ClassName),
    !.

check_new_parts([Field | T], ClassName):-
    Field =.. [field, FieldName, Value],
    atomic_check(FieldName),
    type_of(Value, _),
    check_new_parts(T, ClassName),
    !.

% Value è un'istanza intera

check_new_parts([Field | T], ClassName):-
    Field =.. [field, FieldName, Value, Type],
    atomic_check(FieldName),
    is_instance(Value, Type),
    inst(InstanceName, Value),
    assertz(instance_in_class(ClassName,
                           InstanceName,
                           Type)),
    check_new_parts(T, ClassName),
    !.

% Value è il nome di un'istanza

check_new_parts([Field | T], ClassName):-
    Field =.. [field, FieldName, Value, Type],
    atomic_check(FieldName),
    is_instance(Value, Type),
    assertz(instance_in_class(ClassName,
                           Value,
                           Type)),
    check_new_parts(T, ClassName),
    !.

check_new_parts([Field | T], ClassName):-
    Field =.. [field, FieldName, Value, Type],
    atomic_check(FieldName),
    type_of(Value, Type),
    check_new_parts(T, ClassName),
    !.

check_new_parts([Method | T], ClassName) :-
    Method =.. [method, MethodName, [], Form],
    atom(MethodName),
    NewMethod =.. [MethodName, InstanceName],
    asserta((NewMethod :-
            is_instance(InstanceName, ClassName),
            substitute_this(Form,
                            InstanceName,
                            NewForm),
            call(NewForm),
            !)),
    check_new_parts(T, ClassName),
    !.

check_new_parts([Method | T1], ClassName) :-
    Method =.. [method, MethodName, [Arg | T2], Form],
    atom(MethodName),
    NewMethod =.. [MethodName, InstanceName, Arg | T2],
    asserta((NewMethod :-
            is_instance(InstanceName, ClassName),
            substitute_this(Form,
                            InstanceName,
                            NewForm),
            call(NewForm),
            !)),
    check_new_parts(T1, ClassName).

% type_of/2
% Controlla se il valore è del tipo indicato.

type_of(Value, string) :-
    string(Value),
    !.

type_of(Value, number) :-
    number(Value),
    !.

type_of(Value, integer) :-
    integer(Value),
    !.

type_of(Value, float) :-
    float(Value),
    !.

type_of(Value, rational) :-
    rational(Value),
    rational(Value, _, _),
    !.

type_of(Value, atom) :-
    atom(Value),
    !.

type_of(Value, ClassName) :-
    is_instance(Value, ClassName).

% substitute_this/3
% Ricrea la lista delle clausole all'interno di un metodo
% con tutti i this sostituiti.

substitute_this(Form, InstanceName, NewForm) :-
    \+ is_list(Form),
    Form =.. FormList,
    substitute_this(FormList, InstanceName, FormFinal),
    NewForm =.. FormFinal,
    !.

substitute_this([MethodName | Parameters],
                InstanceName,
                [MethodName | NewParameters]) :-
    MethodName \= ',',
    maplist(param_check, Parameters),
    replace_list(this,
                 Parameters,
                 InstanceName,
                 NewParameters),
    !.

substitute_this([',', Clause, ClauseToCheck],
                InstanceName,
                [',', NewClause, OtherClause]) :-
    Clause =.. [PredName | Parameters],
    maplist(param_check, Parameters),
    replace_list(this,
                 Parameters,
                 InstanceName,
                 NewParameters),
    NewClause =.. [PredName | NewParameters],
    ClauseToCheck =.. CheckingClause,
    substitute_this(CheckingClause,
                    InstanceName,
                    OtherClauseT),
    OtherClause =.. OtherClauseT,
    !.
substitute_this([',', Clause, ClauseToCheck],
                InstanceName,
                [',', NewClause, OtherClause]) :-
    Clause =.. [PredName | Parameters],
    substitute_this(Parameters,
                    InstanceName,
                    NewParameters),
    NewClause =.. [PredName | NewParameters],
    ClauseToCheck =.. CheckingClause,
    substitute_this(CheckingClause,
                    InstanceName,
                    OtherClauseT),
    OtherClause =.. OtherClauseT,
    !.

substitute_this([ClauseBefore, Clause, ClauseToCheck],
                InstanceName,
                [ClauseBefore, NewClause, OtherClause]) :-
    Clause =.. [PredName | Parameters],
    maplist(param_check, Parameters),
    replace_list(this,
                 Parameters,
                 InstanceName,
                 NewParameters),
    NewClause =.. [PredName | NewParameters],
    ClauseToCheck =.. CheckingClause,
    substitute_this(CheckingClause,
                    InstanceName,
                    OtherClauseT),
    OtherClause =.. OtherClauseT,
    !.
% param_check/4
% Controlla che gli argomenti del metodo siano del tipo corretto.

param_check(X) :-
    var(X),
    !.

param_check(X) :-
    type_of(X, _),
    !.

param_check(X) :-
    is_list(X).

% replace_list/4
% Sostituisce tutti i this all'interno della lista degli
% attributi e metodi. Se è una variabile, non lo sostituisce.

replace_list(_, [], _, []) :- !.


replace_list(Old, [Head | T1],  New, [Head | T2]) :-
    var(Head),
    replace_list(Old, T1, New, T2),
    !.

replace_list(Old, [Old | T1], New, [New | T2]) :-
    replace_list(Old, T1, New, T2),
    !.

replace_list(Old, [Head | T1],  New, [Head | T2]) :-
    replace_list(Old, T1, New, T2).


% field_type_check/3
% Ricostruisce gli attributi in base agli attributi
% delle superclassi.

field_type_check(_, [], []).

field_type_check(ParentsParts,
                 [ClassField | T1],
                 [NewField | T2]) :-
    type_check(ParentsParts, ClassField, NewField),
    field_type_check(ParentsParts, T1, T2).

% type_check/3
% Ricostruisce ogni singolo field

% Field è un'istanza intera

type_check([], ClassField, NewField) :-
    ClassField =.. [field, FieldName, FieldValue],
    inst(InstanceName, FieldValue),
    NewField =.. [field, FieldName, InstanceName],
    !.

type_check([], ClassField, NewField) :-
    ClassField =.. [field, FieldName, FieldValue, FieldType],
    inst(InstanceName, FieldValue),
    NewField =.. [field, FieldName, InstanceName, FieldType],
    !.

type_check([], ClassField, ClassField) :-
    !.

% Field è un'istanza intera

type_check([ParentField | _],
           ClassField,
           NewField) :-
    ParentField =.. [field, FieldName, _],
    ClassField =.. [field, FieldName, FieldValue],
    inst(InstanceName, FieldValue),
    NewField =.. [field, FieldName, InstanceName],
    !.

type_check([ParentField | _],
           ClassField,
           ClassField) :-
    ParentField =.. [field, FieldName, _],
    ClassField =.. [field, FieldName, _],
    !.


type_check([ParentField | _],
           ClassField,
           NewField) :-
    ParentField =.. [field, FieldName, _, ParentType],
    ClassField =.. [field, FieldName, FieldValue],
    type_of(FieldValue, ParentType),
    NewField =.. [field,
                       FieldName,
                       FieldValue,
                       ParentType],
    !.

% Field è un'istanza intera

type_check([ParentField | _],
           ClassField,
           NewField) :-
    ParentField =.. [field, FieldName, _, ParentType],
    ClassField =.. [field, FieldName, FieldValue, FieldType],
    is_class(ParentType),
    is_class(FieldType),
    parent_list(FieldType, ParentList),
    inst(InstanceFieldName, FieldValue),
    member(ParentType, ParentList),
    NewField =.. [field,
                  FieldName,
                  InstanceFieldName,
                  FieldType],
        !.

% Field è il nome di un'istanza

type_check([ParentField | _],
           ClassField,
           ClassField) :-
    ParentField =.. [field, FieldName, _, ParentType],
    ClassField =.. [field, FieldName, _, FieldType],
    is_class(ParentType),
    is_class(FieldType),
    parent_list(FieldType, ParentList),
    member(ParentType, ParentList),
    !.


type_check([ParentField | _],
           ClassField,
           ClassField) :-
    ParentField =.. [field, FieldName, _, ParentType],
    ClassField =.. [field, FieldName, _, FieldType],
    subtypep(FieldType, ParentType),
    !.

type_check([ParentField | _], ClassField, _) :-
    ParentField =.. [field, FieldName, _, ParentType],
    ClassField =.. [field, FieldName, _, FieldType],
    format("Error: ~a is not a subtype of  ~a",
           [FieldType, ParentType]),
    !,
    fail.

type_check([ParentField | _], ClassField,  _) :-
    ParentField =.. [field, FieldName, _, ParentType],
    ClassField =.. [field, FieldName, FieldValue],
    format("Error: ~w is not a ~w",
           [FieldValue, ParentType]),
    !,
    fail.

type_check([_ | T], ClassField, NewField) :-
    type_check(T, ClassField, NewField).

% subtypep/2
% Ritorna true se il primo è un sottotipo del secondo.

subtypep(integer, integer).
subtypep(integer, number).
subtypep(integer, float).
subtypep(integer, rational).
subtypep(integer, atomic).

subtypep(float, float).
subtypep(float, number).
subtypep(float, atomic).

subtypep(rational, rational).
subtypep(rational, number).
subtypep(rational, atomic).

subtypep(number, number).
subtypep(number, atomic).

subtypep(string, string).
subtypep(string, atom).
subtypep(string, atomic).

subtypep(atom, atom).
subtypep(atom, atomic).

subtypep(atomic, atomic).

% check_parts/3
% Genera la lista finale degli attributi e metodi.

check_parts([], ClassParts, ClassParts) :- !.

check_parts(ParentsParts, [], ParentsParts) :- !.

check_parts([ParentPart | T],
            ClassParts,
            PartsFinal) :-
    check_list(ParentPart,
               ClassParts,
               [ParentPart]),
    check_parts(T, ClassParts, PartsFinal),
    !.

check_parts([ParentPart | T],
            ClassParts,
            [ParentPart | PartsFinal]) :-
    check_list(ParentPart, ClassParts, []),
    check_parts(T, ClassParts, PartsFinal).

% check_list/3
% Ritorna una lista con gli attributi e metodi della classe
% padre da sovrascrivere.

check_list(_, [], []) :- !.

check_list(ParentPart, [ClassPart | _], [ParentPart]) :-
    ParentPart =.. [PartType | [PartName | _]],
    ClassPart =.. [PartType | [PartName | _]],
    !.

check_list(ParentPart, [_ | T], PartsToRemove) :-
    check_list(ParentPart, T, PartsToRemove).

% make/2 e make/3
% Crea l'istanza di una classe e la memorizza nella base di
% conoscenza.

make(InstanceName, ClassName) :-
    make(InstanceName, ClassName, []),
    !.

make(InstanceName, ClassName, FieldList) :-
    atomic_check(InstanceName),
    is_class(ClassName),
    \+ is_instance(InstanceName),
    findall(X,
            class(ClassName, _, X),
            [PartsList]),
    instance_field(FieldList, PartsList),
    assertz(instance(InstanceName,
                     ClassName,
                     FieldList)),
    !.

make(InstanceName, ClassName, FieldList) :-
    atomic_check(InstanceName),
    is_class(ClassName),
    is_instance(InstanceName),
    findall(X,
            instance(InstanceName, X, _),
            [OldClass]),
    ClassName \= OldClass,
    findall(Y,
            class(ClassName, _, Y),
            [PartsList]),
    instance_field(FieldList, PartsList),
    findall([X, InstanceName, Z],
            instance_in_class(X,
                              InstanceName,
                              Z),
            List),
    defined_inst_check(List, ClassName),
    retract(instance(InstanceName, _, _)),
    assertz(instance(InstanceName,
                     ClassName,
                     FieldList)),
    format("Instance called ~a has been ridefined",
           [InstanceName]),
    !.

make(InstanceName, ClassName, FieldList) :-
    atomic_check(InstanceName),
    is_class(ClassName),
    is_instance(InstanceName),
    findall(X,
            instance(InstanceName, _, X),
            [OldFieldList]),
    OldFieldList \= FieldList,
    findall(Y,
            class(ClassName, _, Y),
            [PartsList]),
    instance_field(FieldList, PartsList),
    findall([X, InstanceName, Z],
            instance_in_class(X,
                              InstanceName,
                              Z),
            List),
    defined_inst_check(List, ClassName),
    retract(instance(InstanceName, _, _)),
    assertz(instance(InstanceName,
                     ClassName,
                     FieldList)),
    format("Instance called ~a has been ridefined",
           [InstanceName]),
    !.

make(Variable, ClassName, FieldList) :-
    var(Variable),
    \+ var(ClassName),
    \+ var(FieldList),
    is_class(ClassName),
    findall(X,
            class(ClassName, _, X),
            [PartsList]),
    instance_field(FieldList, PartsList),
    Variable = instance(generic, ClassName, FieldList),
    !.

make(InstanceName, ClassName, FieldList) :-
    bagof(instance(InstanceName, ClassName, FieldList),
          instance(InstanceName, ClassName, FieldList),
          Result),
    member(instance(InstanceName, ClassName, FieldList),
           Result).

% defined_inst_check/3
% Controlla se l'istanza ridefinita sia del tipo indicato
% in eventuali field di classi esistenti.

defined_inst_check([], _).

defined_inst_check([[_, _, FieldInstClass] | T],
                    DefinedInstClass) :-
    parent_list(FieldInstClass, ParentList),
    once(member(DefinedInstClass, ParentList)),
    defined_inst_check(T, DefinedInstClass),
    !.

defined_inst_check([[ClassName, _, FieldInstClass] | _],
                    DefinedInstClass) :-
    format("Error: In class ~w you defined a field with ~w,
    but instance is now a ~w",
           [ClassName, FieldInstClass, DefinedInstClass]),
    !,
    fail.

% instance_field/2
% Controlla che gli attributi dell`istanza esistano
% nella classe.

instance_field([], _) :- !.

instance_field([Head | Tail], PartsList) :-
    Head =.. [=, FieldName, _],
    once(member(field(FieldName, _), PartsList)),
    instance_field(Tail, PartsList),
    !.

instance_field([Head | Tail], PartsList) :-
    Head =.. [=, FieldName, InstanceValue],
    once(member(field(FieldName, _, Type), PartsList)),
    type_of(InstanceValue, Type),
    instance_field(Tail, PartsList).

% is_class/1
% Controlla se il parametro è una classe esistente.

is_class(ClassName):-
    atomic_check(ClassName),
    !,
    findall(class(ClassName, _,_),
            class(ClassName, _,_),
            ClassList),
    ClassList \= [].

% is_instance/1
% Controlla se è un'istanza qualunque.

is_instance(InstanceName) :-
    atomic_check(InstanceName),
    inst(InstanceName, _),
    !.

is_instance(Value) :-
    inst(_, Value),
    !.

% is_instance/2
% Controlla se è un'istanza della classe.

is_instance(Value, ClassName) :-
    atomic_check(Value),
    inst(Value, Instance),
    is_instance(Instance, ClassName),
    !.

is_instance(Value, ClassName) :-
    Value =.. [instance, InstanceName, ClassName, _],
    inst(InstanceName, Value),
    !.

is_instance(Value, ClassName) :-
    atomic_check(Value),
    inst(Value, Instance),
    is_instance(Instance, ClassName),
    !.

is_instance(Value, ClassName) :-
    Value =.. [instance, _, InstanceClass, _],
    parent_list(InstanceClass, ParentList),
    member(ClassName, ParentList).

% parent_list/2
% Ritorna la lista di tutte le superclassi di Class.

parent_list([], []).

parent_list(Class, [Class | Result]) :-
    findall(Parent,
            class(Class, Parent, _),
            [Parent]),
    parent_list(Parent, ResultDouble),
    sort(ResultDouble, Result),
    !.

parent_list([H | T], [H | Result]) :-
    findall(Parent,
            class(H, Parent, _),
            [Parent]),
    parent_list(Parent, L1),
    parent_list(T, L2),
    append(L1, L2, ResultDouble),
    sort(ResultDouble, Result).

% inst/2
% Ritorna true se InstanceName è il nome dell'istanza
% Instance.

inst(InstanceName, Instance) :-
    Instance =.. [instance, InstanceName, X, Y],
    findall(instance(InstanceName, X, Y),
            instance(InstanceName, X, Y),
            [Instance]).

% field/3
% Estrae il valore di un campo di una classe.

field(InstanceName, FieldName, Result) :-
    atomic_check(InstanceName),
    inst(InstanceName, Instance),
    field(Instance, FieldName, Result),
    !.

field(Instance, FieldName, Result) :-
    is_instance(Instance),
    atomic_check(FieldName),
    Instance =.. [instance, _, _, InstanceFieldList],
    get_field(InstanceFieldList, FieldName, Result),
    !.

field(Instance, FieldName, Result) :-
    is_instance(Instance),
    atomic_check(FieldName),
    Instance =.. [instance, _, ClassName, _],
    findall(X,
            class(ClassName, _, X),
            [PartsList]),

    get_field(PartsList, FieldName, Result).

% get_field/3
% Restituisce il valore dell'attributo.

get_field([FieldList | _], FieldName, Instance) :-
    FieldList =.. [field, FieldName, InstanceName],
    inst(InstanceName, Instance),
    !.

get_field([FieldList | _], FieldName, FieldValue) :-
    FieldList =.. [field, FieldName, FieldValue],
    !.

get_field([FieldList | _], FieldName, Instance) :-
    FieldList =.. [field, FieldName, InstanceName, _],
    inst(InstanceName, Instance),
    !.

get_field([FieldList | _], FieldName, FieldValue) :-
    FieldList =.. [field, FieldName, FieldValue, _],
    !.

get_field([FieldList | _], FieldName, Instance) :-
    FieldList =.. [=, FieldName, InstanceName],
    inst(InstanceName, Instance),
    !.

get_field([FieldList | _], FieldName, FieldValue) :-
    FieldList =.. [=, FieldName, FieldValue],
    !.

get_field([_ | Tail], FieldName, FieldValue) :-
    get_field(Tail, FieldName, FieldValue).


% fieldx/3

fieldx(R, [], R) :- !.

fieldx(Instance, [Head | Tail], R) :-
    field(Instance, Head, X),
    fieldx(X, Tail, R).

%%%% end of file -- oop.pl --
