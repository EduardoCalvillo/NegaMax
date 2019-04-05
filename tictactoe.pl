	/*********************************
	DESCRIPTION DU JEU DU TIC-TAC-TOE
	*********************************/

	/*
	Une situation est decrite par une matrice 3x3.
	Chaque case est soit un emplacement libre, soit contient le symbole d'un des 2 joueurs (o ou x)

	Contrairement a la convention du tp pr�c�dent, pour mod�liser une case libre
	dans une matrice on n'utilise pas une constante sp�ciale (ex : nil, 'vide', 'libre','inoccupee' ...);
	On utilise plut�t une variable libre (_), c'est�-dire un terme non instanci� ('_').
	La situation initiale est donc une matrice 3x3 composee uniquement de variables libres (_). 
	Ceci est possible car le jeu consiste � instancier la grille avec des symboles et non � d�placer les symbles d�j� affect�s.
	
	
	
	Jouer un coup, c-a-d placer un symbole dans une grille S1 ne consiste pas � g�n�rer une nouvelle grille S2 obtenue 
	en copiant d'abord S1 puis en remplacant le symbole de case libre par le symbole du joueur, mais plus simplement
	� INSTANCIER (au sens Prolog) la variable libre qui repr�sentait la case libre par la valeur associ�e au joueur, ex :
	Case = Joueur, ou a realiser indirectement cette instanciation par unification via un pr�dicat comme member/2, select/3, nth1/3 ...
	
	Ainsi si on joue un coup en S, S perd une variable libre, mais peut continuer � s'appeler S (on n'a pas besoin de la d�signer
	par un nouvel identificateur).
	La situation initiale est une "matrice" 3x3 (liste de 3 listes de 3 termes chacune)
	o� chacun des 9 termes est une variable libre.	
	*/
%:-lib(listut).
situation_initiale([ [_,_,_],
                     [_,_,_],
                     [_,_,_] ]).

situation([ [_,_,_],
            [_,x,_],
            [_,_,_]  ]).
	% Convention (arbitraire) : c'est x qui commence

joueur_initial(x).


	% Definition de la relation adversaire/2

adversaire(x,o).
adversaire(o,x).


	/****************************************************
	 DEFINIR ICI � l'aide du pr�dicat ground/1 comment
	 reconnaitre une situation terminale dans laquelle il
	 n'y a aucun emplacement libre : aucun joueur ne peut
	 continuer � jouer (quel qu'il soit).
	 ****************************************************/


situation_terminale(_Joueur, Situation) :-   
    ground(Situation).

/***************************
 DEFINITIONS D'UN ALIGNEMENT
 ***************************/

alignement(_, []):- false.

alignement(L, Matrix) :- ligne(    L,Matrix).
alignement(C, Matrix) :- colonne(  C,Matrix).
alignement(D, Matrix) :- diagonale(D,Matrix).
	/********************************************
	 DEFINIR ICI chaque type d'alignement maximal 
 	 existant dans une matrice carree NxN.

	 ********************************************/

ligne(L,[L|_]).
ligne(L,[_|M]):-
	ligne(L,M).

colonne([E|R],[L|M]):- 
    nth1(X,L,E),  %Trouver la colonne où on cherchera et l'utiliser comme parametre pour le futur
    colonnes(X,R,M).

colonnes(_,[],[]).
colonnes(X,[E|R],[L|M]):-
    nth1(X,L,E), %Cherche le symbole dans la position trouvé avant
    colonnes(X,R,M).

/* D�finition de la relation liant une diagonale D � la matrice M dans laquelle elle se trouve.
		il y en a 2 sortes de diagonales dans une matrice carree(https://fr.wikipedia.org/wiki/Diagonale) :
		- la premiere diagonale (principale) (descendante) : (A I)
		- la seconde diagonale  (ascendante)  : (R Z)
		A . . . . . . . Z
		. \ . . . . . / .
		. . \ . . . / . .
		. . . \ . / . . .
		. . . . X . . .
		. . . / . \ . . . 
		. . / . . . \ . .
		. / . . . . . \ .
		R . . . . . . . I
	*/
reverser([],Z,Z).
reverser([H|T], Rev_list, Acc):-
	reverser(T, Rev_list, [H|Acc]).

diagonale(D, M) :- premiere_diag(1,D,M).
diagonale(D, M) :- reverser(M,RevM,[]), premiere_diag(1,D,RevM). % Pour faire soit premiere ou deuxieme, jamais les deux comme solution)

premiere_diag(_,[],[]).
premiere_diag(K,[E|D],[Ligne|M]) :-
	nth1(K,Ligne,E),
	K1 is K+1,
	premiere_diag(K1,D,M).

	/***********************************
	 DEFINITION D'UN ALIGNEMENT POSSIBLE
	 POUR UN JOUEUR DONNE
	 **********************************/

possible([X|L], J) :- unifiable(X,J), possible(L,J).

possible([   ], _).

	/* Attention 
	il faut juste verifier le caractere unifiable
	de chaque emplacement de la liste, mais il ne
	faut pas realiser l'unification.
	*/



% A FAIRE 
unifiable(X,J) :- 
	(not(ground(X)) ->
	true
	;
	adversaire(J,A),
	A \= X
	).
	
	/**********************************
	 DEFINITION D'UN ALIGNEMENT GAGNANT
	 OU PERDANT POUR UN JOUEUR DONNE J
	 **********************************/

	/*
	Un alignement gagnant pour J est un alignement
possible pour J qui n'a aucun element encore libre.
Un alignement perdant pour J est gagnant
pour son adversaire.
	*/

% A FAIRE
alignement_gagnant([],_).
alignement_gagnant([E|R], J) :- 
	ground(E),
	E=J,
	alignement_gagnant(R,J).

alignement_perdant([],_).
alignement_perdant([E|R],J):-
	ground(E),
	adversaire(J,A),
	E=A,
	alignement_perdant(R,J).

	/******************************
	DEFINITION D'UN ETAT SUCCESSEUR
	*******************************/

     /*Il faut definir quelle op�ration subit une matrice M representant la situation courante
	lorsqu'un joueur J joue en coordonnees [L,C]
     */	

% A FAIRE

replace([H|T], 1, X, [X|T]):- not(ground(H)). 
replace([H|T], I, X, [H|R]):- I > -1, NI is I-1, replace(T, NI, X, R), !.


%%%%%%% CHANGER!!! Il ne marche pas si on lui donne [L,C] inconnues!!!
successeur(_,[],_,[],_).
successeur(J,[Ligne_etat|Reste_etat],[L,C],[Ligne_resultat|Reste_resultat], Niveau) :- 
	( Niveau \= L -> Ligne_resultat = Ligne_etat, NiveauProchain is Niveau + 1, successeur(J,Reste_etat,[L,C],Reste_resultat, NiveauProchain)
	;
	replace(Ligne_etat,C,J,Ligne_resultat), Reste_resultat = Reste_etat
	).

/* Ceci ne veut pas marcher bien
successeur1(_,[],_,[],_).
successeur1(J,[LE|RE],[L,C],[LR|RR],L):-
    replace(LE,C,J,LR), RR = RE.
successeur1(J,[LE|RE],[L,C],[LR|RR],N):-
    Np is N+1, LR = LE, successeur1(J,RE,[L,C],RR,Np).
*/

	/**************************************
   	 EVALUATION HEURISTIQUE D'UNE SITUATION
  	 **************************************/

/*
1/ l'heuristique est +infini si la situation J est gagnante pour J
2/ l'heuristique est -infini si la situation J est perdante pour J
3/ sinon, on fait la difference entre :
	   le nombre d'alignements possibles pour J
	moins
 	   le nombre d'alignements possibles pour l'adversaire de J
*/


heuristique(J,Situation,H) :-		% cas 1
   H = 10000,				% grand nombre approximant +infini
   alignement(Alig,Situation),
   alignement_gagnant(Alig,J), !.
	
heuristique(J,Situation,H) :-		% cas 2
   H = -10000,				% grand nombre approximant -infini
   alignement(Alig,Situation),
   alignement_perdant(Alig,J),!.	


% on ne vient ici que si les cut precedents n'ont pas fonctionne,
% c-a-d si Situation n'est ni perdante ni gagnante.

% A FAIRE 					cas 3
heuristique(J,Situation,H):-
    findall(Ali,alignement(Ali,Situation),L),
    heuristiqueP(J,L,H).

heuristiqueP(_,[],0).
heuristiqueP(J,[L|S],H) :-  %Attention je boucle avec chaque Ligne!! pas avec chaque alignement
     heuristiqueP(J,S,Hnew),
    adversaire(J,A),
    (   possible(L,J) , not(possible(L,A))->  %Seul Joeur possible
    H is Hnew+1
    ; (   possible(L,A) , not(possible(L,J)) -> %Seul Adversaire possible
      H is Hnew-1
      )
    ;  %Aucun joueur possible ou les deux possibles, alors (0+0 = 0) oú (1-1 = 0)
        H is Hnew
    ).