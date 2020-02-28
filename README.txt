VLAD ANDREI-ALEXANDRU
321CB

================================== Tema 1 PP ==================================

								ClassState.h
-------------------------------------------------------------------------------

	Pentru a realiza primul task am folosit structura de date Map permitandu-mi
o inserare si accesare mult mai usoara a informatiilor prelucrate.

	Am ales sa dau forma unui classState o pereche de doua Map-uri:
	
	1) primul Map contine ca si cheie numele variabile, iar valoarea este
	reprezentata de tipul acesteia.

	2) al doilea Map contine ca si cheie numele functiei, iar valoarea este
	o lista de forma: tipReturnat:(lista de argumente)


	- Functia "initEmptyClass" intoarce o simpla pereche cu doua Map-uri goale
	
	- Functia "insertIntoClass" verifica daca se doreste inserarea unei variabile
	sau a unei functii. Pentru variabila se insereaza in primul Map conform 1).
	Pentru functie se insereaza in al doilea Map conform 2)

	- Functia "getValues" verifica daca se doreste lista cu variabile sau cea
	cu functii. 
		Pentru variabile fac un query pentru lista de perechi 
		(cheie, valoare), adica (nume variabila, tip returnat). Lista returnata
		va fi convertita in lista de liste de String-uri (desfac perechea cu o
		functie lambda).

		Pentru functii fac acelasi lucru, doar ca numele functiei va fi adaugat
		in lista tipReturnat:(argumente)



									Parser.h
-------------------------------------------------------------------------------
	Deoarece am avut probleme cu interpretarea functiilor si a variabilelor la
prima incercare am decis sa schimb structura Program-ului si sa nu ma mai
folosesc de ce am implementat in ClassState (pe parcurs am realizat ca problema
era din alta parte si se putea folosi in continuare ce am facut la primul 
subpunct, dar am continuat cu noua implementare).

	Program-ul este de fapt o pereche cu lista de liste de String-uri (adica
o lista cu toate variabilele sub forma de lista) si un Map care foloseste 
numele clasei ca si cheie si valoare - o pereche de lista de liste pentru 
functii (la fel ca la variabile) si un String pentru numele superclasei.

	Program = ([variabile], Map numeClasa ([functii], superclasa))


	Instructiunea am consiferat-o ca o lista de String-uri:

	Functia "initEmptyProgram" initializeaza un program care are o lista
	goala de variabile ca fst, iar ca snd un Map care contine clasa
	"Global", o lista goala pentru functii si superclasa "Global".

	Functia "getVars" intoarce primul element din pereche, adica lista cu 
	variabile.

	Functia "getClasses" interogheaza map-ul pentru lista de chei, adica 
	lista cu toate numele de clase.

	Functia "getParentClass" cauta clasa dorita in Map si intoarce al doilea 
	element din pereche, adica numele superclasei.

	Functia "getFuncsForClass" intoarce lista goala daca numele clasei nu 
	exista in Map. Pentru clasele care exista se cauta in map, se ia 
	valoarea din Just si se intoarce lista de functii stocata la clasa
	corespunzatoare.

	Functia "parse" primeste input-ul din fisier si face o serie de operatii
	pe acesta pentru a-l aduce la o forma cat mai simpla.

	Functia "interpret" executa anumite instructini in functie de keyword-ul
	instructiunii (primul cuvant din lista).
	- se insereaza o clasa noua care extinde alta clasa (doar daca superclasa exista)
	- se insereaza o clasa simpla sau clasa care vrea sa extinda alta clasa, 
	dar nu exista
	- se insereaza o noua variabila doar daca tipul ei exista in lista de clase
	- se insereaza o functie doar daca verificarile necesare sunt indeplinite 
	cu succes
	- se efectueaza infer pe expresia primita
	- daca niciuna din cele de mai sus inseamna ca nu sunt indeplinite conditiile
	necesare pentru instructiunea curenta, deci se va ignora


	Pentru infer (bonus) ma folosesc de 3 functii auxiliare:

	Functia "getExpr" prelucreaza lista de expresii sub forma de String si 
	intoarce lista de expresii.

	Functia "splitByComma" sparge lista de argumente in expresii doar cand numarul parantezelor deschide e egal cu cele inchise.

	Functia "firstLast" sterge '(' si ')' din String-ul cu argumente (primul si
	ultimul char).


	Functia "infer" (punctul c) se foloseste de o functie auxiliara "getVarType"
	pentru cazul cand expresia este variabila.
		Functia "getVarType" intoarce tipul variabilei sau Nothing daca aceasta 
		nu exista
	Daca expresia este functie atunci verific conditiile specificate in enunt
	si apoi calculez rezultatul final. Pentru functii, "infer" foloseste o functie
	auxiliara "getAllFunctionsFrom" care intoarce o lista cu toate functiile
	din clasa ceruta pana la cea mai de sus superclasa (clasa "Global"), adica
	toate functiile din lantul de mostenire. Apoi pe aceasta lista aplic niste
	filtre si obtin toate functiile cu acelasi nume ca cel cautat. Dupa acest pas
	mai aplic un filtru care verifica daca parametrii cautati exista in lista
	de functii filtrata anterior. De asemenea pentru verificarea conditiilor
	mai folosesc doua functii auxiliare "funcExists" (verifica daca numele 
	functiei exista in lista data) si "testParams" care face infer pe fiecare
	expresie parametru si introduce rezultatele intr-o lista, aceasta fiind
	verificata (daca are un element Nothing atunci se termina inferenta pentru
	expresia initiala).


PUNCTAJ:
	Pe masina locala obtin 110/110 puncte.