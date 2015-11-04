move(
estado(no_centro, acima_caixa, no_centro, nao_tem),
pegar_banana,
estado(no_centro, acima_caixa, no_centro,tem)
).
move(
estado(P, no_chao, P, Banana),
subir,
estado(P, acima_caixa, P, Banana)
).
move(
estado(P1, no_chao, P1, Banana),
empurrar(P1, P2),
estado(P2, no_chao, P2, Banana)
).
move(
estado(P1, no_chao, Caixa, Banana),
caminhar(P1, P2),
estado(P2, no_chao, Caixa, Banana)
).
consegue(estado(_, _, _, tem),[]).
consegue(Estado1,[Movimento|Resto]) :- move(Estado1, Movimento, Estado2), consegue(Estado2,Resto).
