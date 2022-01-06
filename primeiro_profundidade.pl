no(1,"ana",[natureza,pintura,musica,sw,porto]).
no(11,"antonio",[natureza,pintura,carros,futebol,lisboa]).
no(12,"beatriz",[natureza,musica,carros,porto,moda]).
no(13,"carlos",[natureza,musica,sw,futebol,coimbra]).
no(14,"daniel",[natureza,cinema,jogos,sw,moda]).
no(21,"eduardo",[natureza,cinema,teatro,carros,coimbra]).
no(22,"isabel",[natureza,musica,porto,lisboa,cinema]).
no(23,"jose",[natureza,pintura,sw,musica,carros,lisboa]).
no(24,"luisa",[natureza,cinema,jogos,moda,porto]).
no(31,"maria",[natureza,pintura,musica,moda,porto]).
no(32,"anabela",[natureza,cinema,musica,tecnologia,porto]).
no(33,"andre",[natureza,carros,futebol,coimbra]).
no(34,"catia",[natureza,musica,cinema,lisboa,moda]).
no(41,"cesar",[natureza,teatro,tecnologia,futebol,porto]).
no(42,"diogo",[natureza,futebol,sw,jogos,porto]).
no(43,"ernesto",[natureza,teatro,carros,porto]).
no(44,"isaura",[natureza,moda,tecnologia,cinema]).
no(200,"sara",[natureza,moda,musica,sw,coimbra]).

no(51,"rodolfo",[natureza,musica,sw]).
no(61,"rita",[moda,tecnologia,cinema]).


ligacao(1,11,10,8).
ligacao(1,12,2,6).
ligacao(1,13,3,2).
ligacao(1,14,1,5).
ligacao(11,21,5,7).
ligacao(11,22,2,4).
ligacao(11,23,2,8).
ligacao(11,24,6,0).
ligacao(12,21,4,9).
ligacao(12,22,3,8).
ligacao(12,23,2,4).
ligacao(12,24,2,4).
ligacao(13,21,3,2).
ligacao(13,22,0,3).
ligacao(13,23,5,9).
ligacao(13,24,2,4).
ligacao(14,21,2,6).
ligacao(14,22,6,3).
ligacao(14,23,7,0).
ligacao(14,24,2,2).
ligacao(21,31,2,1).
ligacao(21,32,2,3).
ligacao(21,33,3,5).
ligacao(21,34,4,2).
ligacao(22,31,5,4).
ligacao(22,32,1,6).
ligacao(22,33,2,1).
ligacao(22,34,2,3).
ligacao(23,31,4,3).
ligacao(23,32,3,5).
ligacao(23,33,4,1).
ligacao(23,34,2,3).
ligacao(24,31,1,5).
ligacao(24,32,1,0).
ligacao(24,33,3,1).
ligacao(24,34,1,5).
ligacao(31,41,2,4).
ligacao(31,42,6,3).
ligacao(31,43,2,1).
ligacao(31,44,2,1).
ligacao(32,41,2,3).
ligacao(32,42,1,0).
ligacao(32,43,0,1).
ligacao(32,44,1,2).
ligacao(33,41,4,1).
ligacao(33,42,1,3).
ligacao(33,43,7,2).
ligacao(33,44,5,3).
ligacao(34,41,3,2).
ligacao(34,42,1,1).
ligacao(34,43,2,4).
ligacao(34,44,1,2).
ligacao(41,200,2,0).
ligacao(42,200,7,2).
ligacao(43,200,2,4).
ligacao(44,200,1,3).

ligacao(1,51,6,2).
ligacao(51,61,7,3).
ligacao(61,200,2,4).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%-- Primeiro em Profundidade --%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dfsForteNivel(Orig,Dest,N,Cam,Sum):-dfs2ForteNivel(Orig,Dest,[Orig],Cam,N,0,0,Sum).

dfs2ForteNivel(Dest,Dest,LA,Cam,_,_,Sum,Sum):-!,reverse(LA,Cam).
dfs2ForteNivel(Act,Dest,LA,Cam,N,C,SAux,Sum):-  C<N,
										   no(NAct,Act,_),
										   (ligacao(NAct,NX,F,_);ligacao(NX,NAct,_,F)), SAux1 is SAux + F,
										   no(NX,X,_),\+ member(X,LA),
										   C1 is C+1, dfs2ForteNivel(X,Dest,[X|LA],Cam,N,C1,SAux1,Sum).


:-dynamic melhor_sol_forte_nivel/3.

caminho_mais_forte_nivel(Orig,Dest,N,Cam,SumF):-
		get_time(Ti),
		(melhor_caminho_forte_nivel(Orig,Dest,N);true),
		retract(melhor_sol_forte_nivel(Cam,SumF,_)),
		get_time(Tf),
		T is Tf-Ti,
		write('Tempo de geracao da solucao:'),write(T),nl.

melhor_caminho_forte_nivel(Orig,Dest,N):-
		asserta(melhor_sol_forte_nivel(_,_,0)),
		dfsForteNivel(Orig,Dest,N,Cam,Sum),
		atualiza_mais_forte_nivel(Cam,Sum),
		fail.

atualiza_mais_forte_nivel(Cam,Sum):-
		((melhor_sol_forte_nivel(_,_,0),!,retract(melhor_sol_forte_nivel(_,_,_)),asserta(melhor_sol_forte_nivel(Cam,Sum,1)))
		;(melhor_sol_forte_nivel(_,SumMax,_), Sum>SumMax,retract(melhor_sol_forte_nivel(_,_,_)), asserta(melhor_sol_forte_nivel(Cam,Sum,1)))).