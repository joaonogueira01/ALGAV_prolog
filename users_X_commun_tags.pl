% Base de Conhecimento

% no(1,ana,[natureza,pintura,musica,sw,porto]).
% no(11,antonio,[natureza,pintura,carros,futebol,lisboa]).
% no(12,beatriz,[natureza,musica,carros,porto,moda]).
% no(13,carlos,[natureza,musica,sw,futebol,coimbra]).
% no(14,daniel,[natureza,cinema,jogos,sw,moda]).
% no(21,eduardo,[natureza,cinema,teatro,carros,coimbra]).
% no(22,isabel,[natureza,musica,porto,lisboa,cinema]).
% no(23,jose,[natureza,pintura,sw,musica,carros,lisboa]).%
% no(24,luisa,[natureza,cinema,jogos,moda,porto]).
% no(31,maria,[natureza,pintura,musica,moda,porto]).%
% no(32,anabela,[natureza,cinema,musica,tecnologia,porto]).
% no(33,andre,[natureza,carros,futebol,coimbra]).
% no(34,catia,[natureza,musica,cinema,lisboa,moda]).
% no(41,cesar,[natureza,teatro,tecnologia,futebol,porto]).
% no(42,diogo,[natureza,futebol,sw,jogos,porto]).
% no(43,ernesto,[natureza,teatro,carros,porto]).
% no(44,isaura,[natureza,moda,tecnologia,cinema]).
% no(200,sara,[natureza,moda,musica,sw,coimbra]).

% no(51,rodolfo,[natureza,musica,sw]).
% no(61,rita,[moda,tecnologia,cinema]).


% ligacao(1,11,10,8).
% ligacao(1,12,2,6).
% ligacao(1,13,-3,-2).
% ligacao(1,14,1,-5).
% ligacao(11,21,5,7).
% ligacao(11,22,2,-4).
% ligacao(11,23,-2,8).
% ligacao(11,24,6,0).
% ligacao(12,21,4,9).
% ligacao(12,22,-3,-8).
% ligacao(12,23,2,4).
% ligacao(12,24,-2,4).
% ligacao(13,21,3,2).
% ligacao(13,22,0,-3).
% ligacao(13,23,5,9).
% ligacao(13,24,-2, 4).
% ligacao(14,21,2,6).
% ligacao(14,22,6,-3).
% ligacao(14,23,7,0).
% ligacao(14,24,2,2).
% ligacao(21,31,2,1).
% ligacao(21,32,-2,3).
% ligacao(21,33,3,5).
% ligacao(21,34,4,2).
% ligacao(22,31,5,-4).
% ligacao(22,32,-1,6).
% ligacao(22,33,2,1).
% ligacao(22,34,2,3).
% ligacao(23,31,4,-3).
% ligacao(23,32,3,5).
% ligacao(23,33,4,1).
% ligacao(23,34,-2,-3).
% ligacao(24,31,1,-5).
% ligacao(24,32,1,0).
% ligacao(24,33,3,-1).
% ligacao(24,34,-1,5).
% ligacao(31,41,2,4).
% ligacao(31,42,6,3).
% ligacao(31,43,2,1).
% ligacao(31,44,2,1).
% ligacao(32,41,2,3).
% ligacao(32,42,-1,0).
% ligacao(32,43,0,1).
% ligacao(32,44,1,2).
% ligacao(33,41,4,-1).
% ligacao(33,42,-1,3).
% ligacao(33,43,7,2).
% ligacao(33,44,5,-3).
% ligacao(34,41,3,2).
% ligacao(34,42,1,-1).
% ligacao(34,43,2,4).
% ligacao(34,44,1,-2).
% ligacao(41,200,2,0).
% ligacao(42,200,7,-2).
% ligacao(43,200,-2,4).
% ligacao(44,200,-1,-3).

% ligacao(1,51,6,2).
% ligacao(51,61,7,3).
% ligacao(61,200,2,4).

% sinonimos('c#','csharp').
% sinonimos(musica,muzica).

users_X_commun_tags(X,List_Result):-
    get_time(T1),
    obter_todas_tags(Todas_Tags),
    findall(Combinacao,combinations(X,Todas_Tags,Combinacao),Combinacoes),
    findall(User,no(_,User,_),Users),
    users_tags_comuns_combinacao(X,Users,Combinacoes),
    findall([Comb,ListUsers],users_combinacao(Comb,ListUsers),List_Result),
    retractall(users_combinacao(_,_)),
    write('Solucao encontrada em '),
    get_time(T2),
    T is T2-T1,write(T),write(' segundos'),nl.

%=== obter users com tags comuns por cada combinaçao ===
users_tags_comuns_combinacao(_,_,[]).
users_tags_comuns_combinacao(X,Users,[Combinacao|Combinacoes]):-
    verificar_sinonimos(Combinacao,Combinacao_Sinonimos),
    users_X_commun_tags2(X,Combinacao_Sinonimos,Users,Users_Com_Tags),
    users_tags_comuns_combinacao(X,Users,Combinacoes),
    !,
    assertz(users_combinacao(Combinacao,Users_Com_Tags)).

%=== obter lista de users com tags em comum da combinação ===
users_X_commun_tags2(_,_,[],[]):-!.
users_X_commun_tags2(X,Tags,[U|Users],Result):-
    no(_,U,User_Tags),
    intersection(Tags, User_Tags,Commun),
    length(Commun, Size),
    Size >= X, !,
    users_X_commun_tags2(X,Tags,Users,Result1),
    append([U], Result1, Result).
users_X_commun_tags2(X,Tags,[_|Users],Result):-
    !,
    users_X_commun_tags2(X,Tags,Users,Result).

%=== Obter todas as tags dos users ===
obter_todas_tags(Tags):-
    findall(User_Tags,no(_,_,User_Tags),Todas_Tags),
    remover_tags_repetidas(Todas_Tags,Tags).

remover_tags_repetidas([],[]).
remover_tags_repetidas([Lista|Todas_Tags],Tags):-
    remover_tags_repetidas(Todas_Tags,Tags1),!,
    union(Lista,Tags1,Tags).

%=== Verificar as tags com sinonimos ===
verificar_sinonimos([],[]):-!.
verificar_sinonimos([Tag|List_Tags],List_Sinonimos):-
    (sinonimos(Tag,Sin);sinonimos(Sin,Tag)),
    verificar_sinonimos(List_Tags,List_Sinonimos1),!,
    append([Tag,Sin], List_Sinonimos1, List_Sinonimos).
verificar_sinonimos([Tag|List_Tags],List_Sinonimos):-
    verificar_sinonimos(List_Tags,List_Sinonimos1),!,
    append([Tag], List_Sinonimos1, List_Sinonimos).

%=== Cominaçoes ===
combinations(0,_,[]).
combinations(N,[X|T],[X|Comb]):-N>0,N1 is N-1,combinations(N1,T,Comb).
combinations(N,[_|T],Comb):-N>0,combinations(N,T,Comb).