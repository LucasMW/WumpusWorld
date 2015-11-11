%metodos dinamicos

:-dynamic([
      agente_local/1,
      agente_orientacao/1,
      wumpus/1,
      abismo/1,
      ouro/1,
      morcego/1,
      tem_ouro/1,
      tem_abismo/1,
      wumpus_vivo/1,
      agente_vivo/1,
      locais_visitados/1,
      pontuacao/1,
      dentro_da_caverna/1,
      flechas/1,
      contagem_ouro/1,
      seguro/1,
     percorrido/1

  ]).

%fatos

tamanho_mundo(6).

parede([X,Y]):-fora_do_mundo([X,Y]).


fora_do_mundo([X,Y]):-
	(tamanho_mundo(Z), (X < 1; X > Z; Y < 1; Y > Z)).

dentro_do_mundo([X,Y]) :- not(fora_do_mundo([X,Y])).



seguro([X,Y]) :- percorrido([X,Y]).
seguro([X,Y]) :- adjacente([X,Y],[A,B]) , tem_brisa([A,B],nao), tem_cheiro([A,B],nao) .
seguro([X,Y]) :- adjacente([X,Y],[A,B]) , tem_cheiro([A,B],sim) , flechas(C), C > 1 .
seguro([X,Y]) :- adjacente([X,Y],[A,B]) , tem_grito([A,B],nao) .
%seguro([X,Y]) :- wumpus_vivo(nao), wumpus_vivo(nao), num_pocosvisitados(3).



matar_wumpus:-
	retract(wumpus_vivo(sim)),
	assert(wumpus_vivo(nao)),
	agente_local([X,Y]),
	retract(wumpus([X,Y])),
	nl,
	write("matou wumpus "),
	write([X,Y]), nl.

matar_agente:-
	retract(agente_vivo(sim)),
	assert(agente_vivo(nao)),
	diminuir_pontuacao(1000),
	nl,
	write("MORREU!!!!!!").

	%percepcao = [brisa,brilho,cheiro,grito,parede].
	%estado(agenteLocal,direcao,qtdFlechas,pontuacao,qtdOuro).

teleportar_agente :-
agente_local([X,Y]),
tamanho_mundo(T),
random_between(1,T, RX),
random_between(1, T, RY),
retract(morcego([X,Y])),
retract(agente_local([X,Y])),
assert(agente_local([RX,RY])),
efeito([RX,RX]). %precisa consultar o efeito do local teleportado

move(
estado(agenteLocal([X,Y]),norte,QtdOuro,QtdFlechas,Pontuacao),
mover_para_frente(norte),
estado(agenteLocal([X,Y1]),Direcao,QtdOuro,QtdFlechas,Pontuacao)
) :- Y1 is Y+1.
move(
estado(agenteLocal([X,Y]),leste,QtdOuro,QtdFlechas,Pontuacao),
mover_para_frente(leste),
estado(agenteLocal([X1,Y]),Direcao,QtdOuro,QtdFlechas,Pontuacao)
) :- X1 is X+1.
move(
estado(agenteLocal([X,Y]),sul,QtdOuro,QtdFlechas,Pontuacao),
mover_para_frente(sul),
estado(agenteLocal([X,Y1]),Direcao,QtdOuro,QtdFlechas,Pontuacao)
) :- Y1 is Y-1.
move(
estado(agenteLocal([X,Y]),oeste,QtdOuro,QtdFlechas,Pontuacao),
mover_para_frente(oeste),
estado(agenteLocal([X1,Y]),Direcao,QtdOuro,QtdFlechas,Pontuacao)
) :- X1 is X-1.
move(
estado(AgenteLocal,norte,QtdOuro,QtdFlechas,Pontuacao),
virar_a_direita(leste),
estado(AgenteLocal,leste,QtdOuro,QtdFlechas,Pontuacao)
).
move(
estado(AgenteLocal,leste,QtdOuro,QtdFlechas,Pontuacao),
virar_a_direita(sul),
estado(AgenteLocal,sul,QtdOuro,QtdFlechas,Pontuacao)
).
move(
estado(AgenteLocal,sul,QtdOuro,QtdFlechas,Pontuacao),
virar_a_direita(oeste),
estado(AgenteLocal,oeste,QtdOuro,QtdFlechas,Pontuacao)
).
move(
estado(AgenteLocal,oeste,QtdOuro,QtdFlechas,Pontuacao),
virar_a_direita(norte),
estado(AgenteLocal,norte,QtdOuro,QtdFlechas,Pontuacao)
).
consegue(estado(_,_,_,_,_,qtdOuro),[]).
consegue(Estado1,[Movimento|Resto]) :- move(Estado1, Movimento, Estado2), consegue(Estado2,Resto).

%inicializar
start :-
iniciar_agente,
iniciar_mundoteste,!.

iniciar_agente :-
	retractall( dentro_da_caverna(_)),
	assert( dentro_da_caverna(sim)),

	retractall( pontuacao(_) ),
	assert(pontuacao(0)),

	retractall(flechas(_)),
	assert(flechas(5)),

	retractall( agente_local(_) ),
	assert( agente_local([1,1]) ),

	retractall( agente_orientacao(_)),
	assert( agente_orientacao(leste)),

	retractall(locais_visitados(_)),
	assert(locais_visitados( [1,1] )),

	retractall(agente_vivo(_)),
	assert(agente_vivo(sim)),

	retractall(contagem_ouro(_)),
	assert(contagem_ouro(0)),

	retractall(percorrido(_)),
	assert(percorrido([1,1])),

	retractall(virada(_,_)).

iniciar_mundoteste :-
	%retractall(tamanho_mundo(_)),
	%assert(tamanho_mundo(6)),

	retractall(wumpus_vivo(_)),
	assert(wumpus_vivo(sim)),

	retractall(tem_abismo(_)),
	assert(tem_abismo(nao)),

	retractall(tem_ouro(_)),
	assert(tem_ouro(nao)),

	retractall(ouro(_)),
	assert(ouro([3,2])),
	assert(ouro([6,5])),
	assert(ouro([1,6])),

	retractall(abismo(_)),
	assert(abismo([1,5])),
	assert(abismo([5,2])),
	assert(abismo([4,2])),
	assert(abismo([5,5])),

	retractall(wumpus(_)),
	assert(wumpus([3,5])),
	assert(wumpus([4,1])),

	retractall(morcego(_)),
	assert(morcego([1,3])),
	%assert(morcego([2,1])),
	assert(morcego([3,3])).


%atualizar a pontuaçao

diminuir_pontuacao(P):-
	pontuacao(X),
	X1 is X - P,
	assert(pontuacao(X1)),
	retract(pontuacao(X)).


aumentar_pontuacao(P):-
	pontuacao(X),
	X1 is X+P,
	assert(pontuacao(X1)),
	retract(pontuacao(X)).


%percepção do agente

adjacente( [X1,Y1], [X2,Y2] ):-	X1 = X2 , Y2 is Y1+1, dentro_do_mundo([X2,Y2]).
adjacente( [X1,Y1], [X2,Y2] ):-	 X1 = X2 , Y2 is Y1-1, dentro_do_mundo([X2,Y2]).
adjacente( [X1,Y1], [X2,Y2] ):- Y1 = Y2 , X2 is X1-1, dentro_do_mundo([X2,Y2]).
adjacente( [X1,Y1], [X2,Y2] ):- Y1 = Y2 , X2 is X1+1, dentro_do_mundo([X2,Y2]).


tem_cheiro([X,Y],sim) :- adjacente([X,Y],[R,T]), wumpus([R,T]).
tem_cheiro([X,Y],nao) :- adjacente([X,Y],[R,T]), not(wumpus([R,T])).
tem_brisa([X,Y],sim) :- adjacente([X,Y], [R,T]), abismo([R,T]).
tem_brisa([X,Y],nao) :- adjacente([X,Y], [R,T]), not(abismo([R,T])).
tem_brilho([X,Y],sim) :-  ouro([X,Y]).
tem_brilho([X,Y],nao) :-  not(ouro([X,Y])).
tem_grito([X,Y],sim) :- adjacente([X,Y], [R,T]), morcego([R,T]).
tem_grito([X,Y],nao) :- adjacente([X,Y], [R,T]), not(morcego([R,T])).
tem_choque([X,Y],sim) :- adjacente([X,Y], [R,T]), parede([R,T]).
tem_choque([X,Y],nao) :- adjacente([X,Y], [R,T]), not(parede([R,T])).

tem_algo([A,B],Z) :- X is A, Y is B,((tem_cheiro([X,Y],sim), Z=cheiro); (tem_brisa([X,Y],sim),Z=brisa); (tem_brilho([X,Y],sim),Z=brilho); (tem_grito([X,Y],sim),Z=grito); (tem_choque([X,Y],sim),Z=parede) ; Z = nada) .

testloop(0).
testloop(N) :- N>0, write("Number : "), write(N), nl, M is N-1, testloop(M).

efeito([X,Y]) :- agente_local([X,Y]),  abismo([X,Y]), nl, write("abismo"), matar_agente.
efeito([X,Y]) :- agente_local([X,Y]),  wumpus([X,Y]), flechas(F), F =< 0 , nl, write("wumpus te devorou"), matar_agente.
efeito([X,Y]) :- agente_local([X,Y]),  wumpus([X,Y]),flechas(F), F > 0 , atirar_flecha ,  nl, write("atirar_flecha é uma boa idéia"),  matar_wumpus.
efeito([X,Y]) :- agente_local([X,Y]),  morcego([X,Y]) , nl, write("morcego te levou"),  teleportar_agente.

%ações do agente

mover_para_frente(leste):-
	agente_vivo(sim),
	agente_orientacao(leste),
	agente_local([X,Y]),
	X1 is X+1, not(fora_do_mundo([X1,Y])),
	assert(agente_local([X1,Y])),
	retract(agente_local([X,Y])),
	%retract(percorrido([X1,Y])),
	assert(percorrido([X1,Y])),
	diminuir_pontuacao(1),
	efeito([X1,Y]),
	! .

mover_para_frente(oeste):-
	agente_vivo(sim),
	agente_orientacao(oeste),
	agente_local([X,Y]),
	X1 is X-1, not(fora_do_mundo([X1,Y])),
	assert(agente_local([X1,Y])),
	retract(agente_local([X,Y])),
	%retract(percorrido([X1,Y])),
	assert(percorrido([X1,Y])),
	diminuir_pontuacao(1),
	efeito([X1,Y]),
	! .


mover_para_frente(norte):-
	agente_vivo(sim),
	agente_orientacao(norte),
	agente_local([X,Y]),
	Y1 is Y+1, not(fora_do_mundo([X,Y1])),
	assert(agente_local([X,Y1])),
	retract(agente_local([X,Y])),
	%retract(percorrido([X,Y1])),
	assert(percorrido([X,Y1])),
	diminuir_pontuacao(1),
	efeito([X,Y1]),
	! .


mover_para_frente(sul):-
	agente_vivo(sim),
	agente_orientacao(sul),
	agente_local([X,Y]),
	Y1 is Y-1, not(fora_do_mundo([X,Y1])),
	assert(agente_local([X,Y1])),
	retract(agente_local([X,Y])),
	%retract(percorrido([X,Y1])),
	assert(percorrido([X,Y1])),
	diminuir_pontuacao(1),
	efeito([X,Y1]),
	! .


virar_a_direita(sul):-
	agente_vivo(sim),
	agente_orientacao(leste),
	retract(agente_orientacao(leste)),
	assert(agente_orientacao(sul)),
	diminuir_pontuacao(1).


virar_a_direita(oeste):-
	agente_vivo(sim),
	agente_orientacao(sul),
	retract(agente_orientacao(sul)),
	assert(agente_orientacao(oeste)),
	diminuir_pontuacao(1).


virar_a_direita(norte):-
	agente_vivo(sim),
	agente_orientacao(oeste),
	retract(agente_orientacao(oeste)),
	assert(agente_orientacao(norte)),
	diminuir_pontuacao(1).



virar_a_direita(leste):-
	agente_vivo(sim),
	agente_orientacao(norte),
	retract(agente_orientacao(norte)),
	assert(agente_orientacao(leste)),
	diminuir_pontuacao(1).



pegar_objeto:-
	agente_vivo(sim),
	agente_local([X,Y]),
	ouro([X,Y]),
	contagem_ouro(Ouro),
	aumentar_pontuacao(1000),
	retract(ouro([X,Y])),
	Ouro1 is Ouro+1,
	retract(contagem_ouro(Ouro)),
	assert(contagem_ouro(Ouro1)),
	assert(tem_brilho([X,Y],nao)).


atirar_flecha:-
	agente_vivo(sim),
	flechas(X),
	X > 0,
	X1 = X-1,
	retract(flechas(X)),
	assert(flechas(X1)),
	diminuir_pontuacao(10).


subir:-
	agente_vivo(sim),
	agente_local([1,1]),
	assert(dentro_da_caverna(nao)).

%propriedades do ambiente

%util evalf(X,Ev) :- Ev is X. %force math evaluation



%printers
showAgent :-
%agente_local([X,Y]), write("["), write(X), write(","), write(Y) , write("]").
agente_local(P), agente_orientacao(O), pontuacao(Score), write("P = "), write(P), write(" \nO = ") ,write(O), write(" \nScore = "),write(Score).
showState :- agente_local([X,Y]),Z = ponto(X,Y), contem(Z,A),write(Z),write(A).

convertToMatrixIndex(N,[X,Y]) :- tamanho_mundo(T), X is mod(N,T)+1,Y is div(N,T)+1 .

showMap :- tamanho_mundo(T), V is T * T-1,showMap(V).
showMap(0):- write([1,1]),tem_algo([1,1],Z) ,write(Z) .
showMap(N) :- N>0, write("Number : "), write(N), convertToMatrixIndex(N,[X,Y]), write([X,Y]), tem_algo([X,Y],Z) ,write(Z) ,M is N-1, nl,showMap(M).


incrementVirada([X,Y]) :-
((virada([X,Y],C), retract(virada([X,Y],C)), CN is C+1) ; (CN = 1)) ,	assert(virada([X,Y],CN)).

%melhor ação

melhor_acao(fim) :-  (agente_vivo(nao), nl, write("lost")); (contagem_ouro(3), nl, write("won")),showAgent,!.
melhor_acao(pegar_objeto) :- pegar_objeto,!.
melhor_acao(mover_para_frente(norte)) :- agente_local([X,Y]), tem_brisa([X,Y],nao), mover_para_frente(norte),!.
melhor_acao(mover_para_frente(leste)) :- agente_local([X,Y]),tem_brisa([X,Y],nao),  mover_para_frente(leste),!.
melhor_acao(mover_para_frente(sul)) :- agente_local([X,Y]),tem_brisa([X,Y],nao), mover_para_frente(sul),!.
melhor_acao(mover_para_frente(oeste)) :- agente_local([X,Y]), tem_brisa([X,Y],nao), mover_para_frente(oeste),!.
melhor_acao(virar_a_direita(A)) :- agente_local([X,Y]), virada([X,Y],C), C =< 4 ,virar_a_direita(A), !.
melhor_acao(atirar_flecha) :- atirar_flecha,! .

run([X|Resto],N) :- M is N+1, write(M), showAgent, nl,melhor_acao(X),write("done: ") ,write(X) , nl , (X=fim;run(Resto,M)).
run :- showAgent, nl,melhor_acao(X),write("done: ") ,write(X) , nl , (X=fim;run).
