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
      contagem_ouro/1
  ]).

%fatos

tamanho_mundo(T).

parede([]).

fora_do_mundo([X,Y]):-
	X < 1; X > 6; Y < 1; Y > 6.

matar_wumpus:-
	retract(wumpus_vivo(sim)),
	assert(wumpus_vivo(nao)).

matar_agente:-
	retract(agente_vivo(sim)),
	assert(agente_vivo(nao)),
	diminuir_pontuacao(1000).

	%percepcao = [brisa,brilho,cheiro,grito,parede].
	%estado(agenteLocal,direcao,qtdFlechas,pontuacao,qtdOuro).
move(
estado(agenteLocal,direcao,qtdOuro,qtdFlechas,pontuacao),
mover_para_frente(direcao),
estado(agenteLocal,direcao,qtdOuro,qtdFlechas,pontuacao)
).
move(
estado(agenteLocal,direcao,qtdOuro,qtdFlechas,pontuacao),
pegar_banana,
estado(no_centro, acima_caixa, no_centro,tem)
).
consegue(estado(_,_,_,_,_,qtdOuro),[]).
consegue(Estado1,[Movimento|Resto]) :- move(Estado1, Movimento, Estado2), consegue(Estado2,Resto).

%inicializar

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
	assert(contagem_ouro(0)).


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

adjacente( [X1,Y1], [X2,Y2] ):-
	( X1 = X2 , Y1 = Y2-1
	; X1 = X2 , Y1 = Y2+1
	; Y1 = Y2 , X1 = X2+1
	; Y1 = Y2 , X1 = X2-1
	).

tem_cheiro([X,Y],sim) :- adjacente([X,Y],[R,T]), wumpus([R,T]).
tem_cheiro([X,Y],nao) :- adjacente([X,Y],[R,T]), not(wumpus([R,T])).
tem_brisa([X,Y],sim) :- adjacente([X,Y], [R,T]), abismo([R,T]).
tem_brisa([X,Y],nao) :- adjacente([X,Y], [R,T]), not(abismo([R,T])).
tem_brilho([X,Y],sim) :- adjacente([X,Y], [R,T]), ouro([R,T]).
tem_brilho([X,Y],nao) :- adjacente([X,Y], [R,T]), not(ouro([R,T])).
tem_grito([X,Y],sim) :- adjacente([X,Y], [R,T]), morcego([R,T]).
tem_grito([X,Y],nao) :- adjacente([X,Y], [R,T]), not(morcego([R,T])).
tem_parede([X,Y],sim) :- adjacente([X,Y], [R,T]), parede([R,T]).
tem_parede([X,Y],nao) :- adjacente([X,Y], [R,T]), not(parede([R,T])).


%ações do agente

mover_para_frente(leste):-
	agente_vivo(sim),
	agente_orientacao(leste),
	agente_local([X,Y]),
	X1 is X+1, not(fora_do_mundo([X1,Y])),
	assert(agente_local([X1,Y])),
	retract(agente_local([X,Y])),
	diminuir_pontuacao(1).

mover_para_frente(oeste):-
	agente_vivo(sim),
	agente_orientacao(oeste),
	agente_local([X,Y]),
	X1 is X-1, not(fora_do_mundo([X1,Y])),
	assert(agente_local([X1,Y])),
	retract(agente_local([X,Y])),
	diminuir_pontuacao(1).

mover_para_frente(norte):-
	agente_vivo(sim),
	agente_orientacao(norte),
	agente_local([X,Y]),
	Y1 is Y+1, not(fora_do_mundo([X,Y1])),
	assert(agente_local([X,Y1])),
	retract(agente_local([X,Y])),
	diminuir_pontuacao(1).

mover_para_frente(sul):-
	agente_vivo(sim),
	agente_orientacao(sul),
	agente_local([X,Y]),
	Y1 is Y-1, not(fora_do_mundo([X,Y1])),
	assert(agente_local([X,Y1])),
	retract(agente_local([X,Y])),
	diminuir_pontuacao(1).



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

virar_a_direta(norte):-
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

	diminuir_pontuacao(1).



flecha_movimento([X,Y],leste):-
	X1 is X+1, not(fora_do_mundo([X1,Y])),
	assert(flecha_movimento([X1,Y],leste)).

flecha_movimento([X,Y],oeste):-
	X1 is X-1, not(fora_do_mundo([X1,Y])),
	assert(flecha_movimento([X1,Y],oeste)).

flecha_movimento([X,Y],norte):-
	Y1 is Y+1, not(fora_do_mundo([X,Y1])),
	assert(flecha_movimento([X,Y1],norte)).

flecha_movimento([X,Y],sul):-
	Y1 is Y-1, not(fora_do_mundo([X,Y1])),
	assert(flecha_movimento([X,Y1],sul)).

flecha_alvo([X,Y],Direcao):-
	flecha_movimento([X,Y],Direcao),
	wumpus([X,Y]),
	matar_wumpus.


atirar_flecha:-
	agente_vivo(sim),
	flechas(X),
	X > 0,
	X1 = X-1,
	retract(flechas(X)),
	assert(flechas(X1)),
	agente_local([R,T]),
	agente_orientacao(Direcao),
	flecha_movimento([R,T],Direcao),
	diminuir_pontuacao(10).


subir:-
	agente_vivo(sim),
	agente_local([1,1]),
	diminuir_pontuacao(1),
	assert(dentro_da_caverna(nao)).

%propriedades do ambiente


%melhor ação


