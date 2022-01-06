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


ligacao(1,11,10,8,2,2).
ligacao(1,12,2,6,2,2).
ligacao(1,13,3,2,2,2).
ligacao(1,14,1,5,2,2).
ligacao(1,51,6,2,2,2).
ligacao(11,21,5,7,2,2).
ligacao(11,22,2,4,2,2).
ligacao(11,23,2,8,2,2).
ligacao(11,24,6,0,2,2).
ligacao(12,21,4,9,2,2).
ligacao(12,22,3,8,2,2).
ligacao(12,23,2,4,2,2).
ligacao(12,24,2,4,2,2).
ligacao(13,21,3,2,2,2).
ligacao(13,22,0,3,2,2).
ligacao(13,23,5,9,2,2).
ligacao(13,24,2,4,2,2).
ligacao(14,21,2,6,2,2).
ligacao(14,22,6,3,2,2).
ligacao(14,23,7,0,2,2).
ligacao(14,24,2,2,2,2).
ligacao(21,31,2,1,2,2).
ligacao(21,32,2,3,2,2).
ligacao(21,33,3,5,2,2).
ligacao(21,34,4,2,2,2).
ligacao(22,31,5,4,2,2).
ligacao(22,32,1,6,2,2).
ligacao(22,33,2,1,2,2).
ligacao(22,34,2,3,2,2).
ligacao(23,31,4,3,2,2).
ligacao(23,32,3,5,2,2).
ligacao(23,33,4,1,2,2).
ligacao(23,34,2,3,2,2).
ligacao(24,31,1,5,2,2).
ligacao(24,32,1,0,2,2).
ligacao(24,33,3,1,2,2).
ligacao(24,34,1,5,2,2).
ligacao(31,41,2,4,2,2).
ligacao(31,42,6,3,2,2).
ligacao(31,43,2,1,2,2).
ligacao(31,44,2,1,2,2).
ligacao(32,41,2,3,2,2).
ligacao(32,42,1,0,2,2).
ligacao(32,43,0,1,2,2).
ligacao(32,44,1,2,2,2).
ligacao(33,41,4,1,2,2).
ligacao(33,42,1,3,2,2).
ligacao(33,43,7,2,2,2).
ligacao(33,44,5,3,2,2).
ligacao(34,41,3,2,2,2).
ligacao(34,42,1,1,2,2).
ligacao(34,43,2,4,2,2).
ligacao(34,44,1,2,2,2).
ligacao(41,200,2,0,2,2).
ligacao(42,200,7,2,2,2).
ligacao(43,200,2,4,2,2).
ligacao(44,200,1,3,2,2).

ligacao(51,61,7,3,2,2).
ligacao(61,200,2,4,2,2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%-- Primeiro em Profundidade --%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dfsForteNivel(Orig,Dest,N,Cam,Sum):-dfs2ForteNivel(Orig,Dest,[Orig],Cam,N,0,0,Sum).

dfs2ForteNivel(Dest,Dest,LA,Cam,_,_,Sum,Sum):-!,reverse(LA,Cam).
dfs2ForteNivel(Act,Dest,LA,Cam,N,C,SAux,Sum):-  C<N,
										   no(NAct,Act,_),
										   (ligacao(NAct,NX,F,_,_,_);ligacao(NX,NAct,_,F,_,_)), SAux1 is SAux + F,
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%-----Subrede de utilizadores----------%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

obterMaiorForcaLigacao(Orig,N,FMax):-
									tamanhoRedeAdaptado(Orig,N,R),
									obterMaiorForcaLigacao2(R,FMax).

obterListaDecrescenteForcasLigacao(Orig,N,LDesc):-
									tamanhoRedeAdaptado(Orig,N,R),
									obterListaDecrescenteForcasLigacao2(R,L1),
									flatten(L1,L2),
									sort(0,@>=,L2,LDesc).

tamanhoRede1Adaptado(_,0,[]) :- !.
tamanhoRede1Adaptado(Orig, Counter, LIST):-
    no(NUM,Orig,_),
    ((ligacao(NUM,New_User_Num,_,_,_,_));(ligacao(New_User_Num,NUM,_,_,_,_))),
    no(New_User_Num,USER,_),
    Counter1 is Counter-1, Counter1 >= 0,
    tamanhoRede1Adaptado(USER,Counter1, LIST1),
    append([USER], LIST1, LIST).


tamanhoRedeAdaptado(Orig, Counter, LIST):-
    findall(LIST1, tamanhoRede1Adaptado(Orig, Counter, LIST1), NEWL),
    flatten(NEWL, LIST1),
	sort([Orig|LIST1], LIST).



obterMaiorForcaLigacao2([_],-100):- !.
obterMaiorForcaLigacao2([H|T],FMax):-
									obterMaiorForcaLigacao2(T,FMax1),
									obterMaiorForcaLigacao3(H,T,FMax2),
									((FMax2>FMax1,!,FMax = FMax2);(FMax = FMax1)).

obterMaiorForcaLigacao3(_,[],-100):- !.
obterMaiorForcaLigacao3(A,[B|T],FMax):-
									no(NumA,A,_),
									no(NumB,B,_),
									(ligacao(NumA,NumB,FAB,FBA,_,_);ligacao(NumB,NumA,FBA,FAB,_,_)),
									!,
									obterMaiorForcaLigacao3(A,T,FMax1),
									((FAB>FBA,!,FMax2 = FAB);(FMax2 = FBA)),
									((FMax2>FMax1,!,FMax = FMax2);(FMax = FMax1)).
obterMaiorForcaLigacao3(A,[_|T],FMax):-
									obterMaiorForcaLigacao3(A,T,FMax).

obterListaDecrescenteForcasLigacao2([_],[]):-!.
obterListaDecrescenteForcasLigacao2([A|T1],[L|T2]):-
													findall([FAB,FBA],
															(member(B,T1),
															no(NumA,A,_),
															no(NumB,B,_),
									                        (ligacao(NumA,NumB,FAB,FBA,_,_);ligacao(NumB,NumA,FBA,FAB,_,_))),
															L),
													obterListaDecrescenteForcasLigacao2(T1,T2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%----- A* star -----%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:-dynamic max_forca/1.

aStar(Orig,Dest,N,Cam,Custo):-
    get_time(T1),

	obterMaiorForcaLigacao(Orig,N,FMax),
	asserta(max_forca(FMax)),

    no(ID_Orig,Orig,_),
    no(ID_Dest,Dest,_),
    aStar2(ID_Dest,[(_,0,[ID_Orig])],N,Cam,Custo),

	retract(max_forca(_)),

    write('Solucao encontrada em '),
    get_time(T2),
    T is T2-T1,write(T),write(' segundos'),nl.

aStar2(Dest,[(_,Custo,[Dest|T])|_],_,Cam,Custo):-
												!,
												reverse([Dest|T],Cam).
aStar2(Dest,[(_,Ca,LA)|Outros],N,Cam,Custo):-
											LA=[Act|_],
											length(LA,C),
											findall((CEX,CaX,[X|LA]),
													(C=<N,
													Dest\==Act,
													(ligacao(Act,X,CustoX,_,_,_);ligacao(X,Act,_,CustoX,_,_)),
													\+ member(X,LA),
													CaX is CustoX + Ca,
													estimativa(X,Dest,C,N,EstX),
													CEX is CaX + EstX),
													Novos),
											append(Outros,Novos,Todos),
											%write('Novos='),write(Novos),nl,
                                            sort(0,@>=,Todos,TodosOrd),
											%write('TodosOrd='),write(TodosOrd),nl,
                                            aStar2(Dest,TodosOrd,N,Cam,Custo).

estimativa(NoDestino,NoDestino,_,_,0):- !.
estimativa(_,_,NivelAtual,NivelDestino,Estimativa):-
													max_forca(FMax),
													Index is NivelDestino - NivelAtual,
													Estimativa is Index * FMax.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%															BEST FIRST								%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




bestfs1(Orig,Dest,N,Cam,Custo):-
	get_time(T1),

    no(NOrig,Orig,_),no(NDest,Dest,_),
	obterMaiorForcaLigacao(Orig,N,FMax),
	asserta(max_forca(FMax)),
	bestfs12(NDest,[[NOrig]],N,Cam,Custo),
	retract(max_forca(_)),
	write('Solucao encontrada em '),
    get_time(T2),
    T is T2-T1,write(T),write(' segundos'),nl.

bestfs12(Dest,[[Dest|T]|_],_,Cam,Custo):- 
	reverse([Dest|T],Cam),
	calcula_custo(Cam,Custo).

bestfs12(Dest,[[Dest|_]|LLA2],N,Cam,Custo):- 
	!,
	bestfs12(Dest,LLA2,N,Cam,Custo).
	

bestfs12(Dest,LLA,N,Cam,Custo):-
	member1(LA,LLA,LLA1),
	LA=[Act|_],
	length(LA,Tamanho),
	((Act==Dest,!,bestfs12(Dest,[LA|LLA1],N,Cam,Custo))
	 ;
	 (
	  findall((Estimativa,[X|LA]),(Tamanho=<N,(ligacao(Act,X,_,_,_,_);ligacao(X,Act,_,_,_,_)),
	  \+member(X,LA),estimativa(Dest,X,Tamanho,N,Estimativa)),Novos),
	  Novos\==[],!,
	  sort(0,@>=,Novos,NovosOrd),
	  retira_custos(NovosOrd,NovosOrd1),
	  append(NovosOrd1,LLA1,LLA2),
	  %write('LLA2='),write(LLA2),nl,
	  bestfs12(Dest,LLA2,N,Cam,Custo)
	 )).

member1(LA,[LA|LAA],LAA).
member1(LA,[_|LAA],LAA1):-member1(LA,LAA,LAA1).

retira_custos([],[]).
retira_custos([(_,LA)|L],[LA|L1]):-retira_custos(L,L1).

calcula_custo([Act,X],C):-!,ligacao(Act,X,C,_,_,_).
calcula_custo([Act,X|L],S):-calcula_custo([X|L],S1), 
							(ligacao(Act,X,FL,_,_,_);ligacao(X,Act,_,FL,_,_)),
							S is S1+FL.