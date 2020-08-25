% Nome: Duarte Calado de Almeida, n. 95565

:- [codigo_comum].

%------------------------------------------------------------------------------%
% obtem_letras_palavras(Lst_Pals, Letras) afirma que Letras e a lista ordenada %
% cujos elementos sao listas com as letras de cada palavra de Lst_Pals         %
%------------------------------------------------------------------------------%

obtem_letras_palavras(Lst_Pals, Letras) :-
    maplist(atom_chars, Lst_Pals, Letras_desord),
    sort(Letras_desord, Letras).

%-------------------------------------------------------------------------------%
% espaco_fila(Fila, Esp) afirm que Esp e um espaco de um Fila (linha ou coluna) %
% de uma grelha, isto e, um conjunto contiguo de pelo menos 3 posicoes livres   %
%-------------------------------------------------------------------------------%

espaco_fila(Fila, Esp) :- espaco_fila(Fila, [], Esp).

% se chegou a um #, provar que o acumulador tem pelo menos 3 elementos
espaco_fila([El|_], Res, Res) :-

    El == #,
    length(Res, Comp),
    Comp >= 3.

% se o acumulador tiver menos de 3 elementos ou se exigir outra sequencia de variaveis
espaco_fila([El|Resto], _, Esp) :-

    El == #,

    % continuar procura, reiniciado o acumulador 
    espaco_fila(Resto, [], Esp).

%caso terminal: nao ha mais elementos na lista
espaco_fila([], Res, Res) :-
    length(Res, Comp),
    Comp >= 3, !.

% se o elemento da lista e uma variavel, atualizar acumulador 
espaco_fila([El|Resto], Ac, Esp) :-

    El \== #,
    append(Ac, [El], Ac_act),

    espaco_fila(Resto, Ac_act, Esp).

%---------------------------------------------------------------------------------------%
% espacos_fila(Fila, Espacos) afirma que Espacos e a lista de todos os espacos em Fila, %
% sendo Fila uma linha ou coluna de uma grelha                                          %
%---------------------------------------------------------------------------------------%

espacos_fila(Fila, Espacos) :-
    bagof(Esp, espaco_fila(Fila, Esp), Espacos),
    !.

% caso nao haja espacos numa fila, Espacos e a lista vazia
espacos_fila(_, []).


%-----------------------------------------------------------------------------------%
% espacos_puzzle(Grelha, Espacos) afirma que Espacos e a lista de todos os espacos  %
% de Grelha, tanto das filas como das colunas                                       %
%-----------------------------------------------------------------------------------%

espacos_puzzle(Grelha, Espacos) :-

    % Filas e a lista de espacos nas filas de Grelhas
    maplist(espacos_fila, Grelha, Esp_Fila),
    alisa(Esp_Fila, Filas),

    % Idem para as Colunas (transpoe-se a matriz)
    mat_transposta(Grelha, Grelha_Trans),

    maplist(espacos_fila, Grelha_Trans, Esp_Col),
    alisa(Esp_Col, Colunas),

    append(Filas, Colunas, Espacos).

% alisa(Lista_1, Lista_2) afirma que Lista_2 resulta de alisar Lista_1 em um nivel de parenteses 
alisa([], []).

alisa([Lista|Resto], Resultado) :-
    alisa(Resto, Resultado_dir),
    append(Lista, Resultado_dir, Resultado).


%-----------------------------------------------------------------------------------%
% espacos_com_posicoes_comuns(Espacos, Esp, Esp_com) afirma que Esps_com e a lista  %
% de espacos com variaveis em comum com Esp, a excecao de Esp                       %
%-----------------------------------------------------------------------------------%

espacos_com_posicoes_comuns(Espacos, Esp, Esp_com) :- 

    % Espacos_filtrados obtem-se excluindo os espacos sem posicoes comuns com Esp 
    exclude(sem_posicoes_comuns(Esp), Espacos, Espacos_filtrados),

    % Esp_com resulta de excluir Esp de Espacos_filtrados 
    exclude(==(Esp), Espacos_filtrados, Esp_com).

% sem_posicoes_comuns(Esp, Espaco) afirma que Espaco e Esp nao tem posicoes comuns
sem_posicoes_comuns(Esp, Espaco) :-

    % Pos_comuns resulta de filtrar as posicoes comuns de Esp e Espaco 
    include(pertence(Esp), Espaco, Pos_comuns),

    % Garantir que nao ha posicoes comuns 
    Pos_comuns == [].

% pertence(Espaco, Pos) afirma que Pos pertence ao Espaco 
pertence([Pos_2|_], Pos_1) :- Pos_1 == Pos_2, !.                             
pertence([_|Resto], Pos) :- pertence(Resto, Pos).


%---------------------------------------------------------------------------------------%
% palavra_possivel_esp(Pal, Esp, Espacos, Letras) afirma que Pal e uma lista de letras  %
% de uma palavra de Letras valida para o espaco Esp de Espacos                          %
%---------------------------------------------------------------------------------------%

palavra_possivel_esp(Pal, Esp, Espacos, Letras) :-                                                         

    % verificar que Esp e Pal tem o mesmo comprimento
    length(Pal, Len),
    length(Esp, Len),

    Pal = Esp,
    espacos_com_posicoes_comuns(Espacos, Esp, Esp_com),                                             
                                                                                                    
    % Verificar se ha palavras compativeis em Palavras para os Esp_com
    espacos_possiveis_aux(Esp_com, Letras).


% espacos_possiveis_aux(Espacos, Palavras) afirma que ha pelo menos uma palavra de Palavras
% que respeita cada espaco de Espacos
espacos_possiveis_aux([], _) :- !.

espacos_possiveis_aux([Espaco|Resto], Palavras) :- 
    bagof(X, (member(X,Palavras), respeita(Espaco, X)), _),                                     
    espacos_possiveis_aux(Resto, Palavras). 


% respeita(Esp, Pal) afirma que e possivel atribuir Pal a Esp
respeita(Espaco, Palavra) :-
    copia(Espaco, Copia),
    Copia = Palavra.


% copia(Esp, Copia) afirma que Copia e uma copia de Esp
copia(Espaco, Copia) :-                                                                           
    maplist(copia_el, Espaco, Copia).

% no caso em que e uma variavel, usar variavel anonima para contornar unificacoes
copia_el(El, _) :-
    var(El), !.

% caso contrario, fazer copia direta
copia_el(El, El).                       


%----------------------------------------------------------------------------------------%
% palavras_possiveis_esp(Letras, Espacos, Esp, Pals_Possiveis) afirma que Pals_Possiveis %
% e uma lista ordenada de todas as palavras possiveis para o espaco Esp                  %
%----------------------------------------------------------------------------------------%

palavras_possiveis_esp(Letras, Espacos, Esp, Pals_Possiveis) :-
    findall(Pal, (member(Pal, Letras), palavra_possivel_esp(Pal, Esp, Espacos, Letras)), Pals_Possiveis).


%-----------------------------------------------------------------------------------------%
% palavras_possiveis(Letras, Espacos, Pals_Possiveis) afirma que Pals_Possiveis e a lista %
% de palavras possiveis pertencentes a Letras, na forma [Espaco, Lista], em que Espaco    %
% pertence a Espacos                                                                      %
%-----------------------------------------------------------------------------------------%

palavras_possiveis(Letras, Espacos, Pals_Possiveis) :-
    maplist(espaco_e_pal_possiveis(Letras, Espacos), Espacos, Pals_Possiveis).

% fazer correspondencia entre Espaco -> [Espaco, Pals_Possiveis] 
espaco_e_pal_possiveis(Letras, Espacos, Espaco, [Espaco, Pals_Possiveis]) :-
    palavras_possiveis_esp(Letras, Espacos, Espaco, Pals_Possiveis).

%--------------------------------------------------------------------------------------------------%
% letras_comuns(Lst_Pals, Letras_comuns) afirma que Letras_comuns e a lista de tuplos (pos, letra) %
% que indica que todas as listas de Lst_Pals tem letra na posicao pos                              %
%--------------------------------------------------------------------------------------------------%

letras_comuns(Lst_Pals, Letras_comuns) :-
    Lst_Pals \== [],
    letras_comuns(Lst_Pals, 1, [], Letras_comuns).

% Fim de iteracao
letras_comuns(Lst_Pals, Count_letra, Letras_comuns, Letras_comuns) :-

    nth1(1, Lst_Pals, Palavra),
    length(Palavra, Len_Palavra),

    Count_letra > Len_Palavra, !.

% caso de igualdade entre letras
letras_comuns(Lst_Pals, Count_letra, Ac, Res) :-

    nth1(1, Lst_Pals, Palavra),
    length(Palavra, Len_Palavra),

    Count_letra =< Len_Palavra,

    % verificar que as letras no mesmo indice/posicao de cada palavra sao iguais 
    letras_iguais_indice(Lst_Pals, Count_letra), !,

    % adicionar tuplo ao acumulador
    nth1(Count_letra, Palavra, Letra),
    append(Ac, [(Count_letra, Letra)], Ac_act),

    Count_letra_act is Count_letra + 1,
    letras_comuns(Lst_Pals, Count_letra_act, Ac_act, Res).

% caso de desigualdade entre letras
letras_comuns(Lst_Pals, Count_letra, Ac, Res) :-

    Count_letra_act is Count_letra + 1,
    letras_comuns(Lst_Pals, Count_letra_act, Ac, Res).


% letras_iguais_indice(Lst_pals, Indice) afirma que cada palavra de Lst_pals tem a mesma letra em Indice
letras_iguais_indice(Lst_pals, Indice) :-

    % Lst_char e a lista de letras das palavras de Lst_pals no indice Indice 
    maplist(nth1(Indice), Lst_pals, Lst_chars),

    % Verificar igualdade de toda a lista com o seu primeiro elemento
    nth1(1, Lst_chars, Char),
    maplist(=(Char), Lst_chars).


%----------------------------------------------------------------------------------%
% atribui_comuns(Pals_Possiveis) afirma que Pals_Possiveis e a lista de palavras   %
% possiveis para cada espaco resultante de atribuir a cada espaco as letras comuns %
% a todas as palavra possiveis para esse espaco                                    %
%----------------------------------------------------------------------------------%

atribui_comuns(Pals_Possiveis) :-
    maplist(preenche, Pals_Possiveis).

% preenche([Espaco,Letras_Possiveis] afirma que espaco tem as letras comuns a Letras_Possiveis
% no mesmo indice
preenche([Espaco, Letras_Possiveis]) :-
    letras_comuns(Letras_Possiveis, Letras_Comuns),
    atribui_comuns_aux(Espaco, Letras_Comuns).

% atribui_comuns_aux(Espaco, Lista_tuplos) afirma que Espaco tem Letra no Indice especificado
% em cada tuplo 
atribui_comuns_aux(_, []) :-  !.

atribui_comuns_aux(Espaco, [(Indice, Letra)|R]) :-
    nth1(Indice, Espaco, Letra),
    !,
    atribui_comuns_aux(Espaco, R).


%----------------------------------------------------------------------------------------------%
% retira_impossiveis(Pals_Possiveis, Novas_Pals_Possiveis) afirma que Novas_Pals_Possiveis e o %
% resultado de retirar palavras impossiveis de Pals_Possiveis, isto e, palavras que deixam de  %
% respeitar o conteudo das posicoes do espaco                                                  %
%----------------------------------------------------------------------------------------------%

retira_impossiveis(Pals_Possiveis, Novas_Pals_Possiveis) :-
    maplist(retira_impossiveis_aux, Pals_Possiveis, Novas_Pals_Possiveis),!.

% Pals_act resulta de retirar de Pals palavras impossiveis para Espaco 
retira_impossiveis_aux([Espaco, Pals], [Espaco, Pals_act]) :-
    bagof(Pal, (member(Pal, Pals), respeita(Espaco, Pal)), Pals_act).    

    
%---------------------------------------------------------------------------------------------%
% obtem_unicas(Pals_possiveis, Unicas) afirma que Unicas e a lista de palavras unicas para um %
% dado espaco em Pals_Possiveis                                                               %
%---------------------------------------------------------------------------------------------%

obtem_unicas(Pals_Possiveis, Unicas) :-

    % Filtrar para Esp_e_Pals_Unicas as listas so com 1 palavra possivel 
    include(filtra_unicas, Pals_Possiveis, Esp_e_Pals_Unicas),

    % fazer correspondencia [Espaco, [Pal_possivel]] -> Pal_possivel 
    maplist(palavra_unica, Esp_e_Pals_Unicas, Unicas).

% Afirma que so ha uma palavra possivel para um espaco 
filtra_unicas([_, Pals_possiveis]) :-
    length(Pals_possiveis, 1).

% Afirma que Palavra e a primeira palavra de Pals_possiveis 
palavra_unica([_, Pals_possiveis], Palavra) :-
    nth1(1, Pals_possiveis, Palavra).


%-------------------------------------------------------------------------------------%
% retira_unicas(Pals_Possiveis, Novas_Pals_Possiveis) afirma que Novas_Pals_Possiveis %
% resulta de retirar de Pals_Possiveis as palavras unicas                             %
%-------------------------------------------------------------------------------------%

retira_unicas(Pals_Possiveis, Novas_Pals_Possiveis) :-
    obtem_unicas(Pals_Possiveis, Pals_Unicas),
    maplist(retira_pals_unicas(Pals_Unicas), Pals_Possiveis, Novas_Pals_Possiveis).

% fazer correspondencia [Espaco, Lista_Pals] -> [Espaco, Lista_Pals_act], em que Lista_Pals_act 
% e a lista que se obtem retirando as palavras de Pals_Unicas de Lista_Pals 
retira_pals_unicas(Pals_Unicas, [Espaco, Lista_Pals], [Espaco, Lista_Pals_act]) :-

    length(Lista_Pals, Len_Lista_Pals),

    % se a lista tiver mais que um elemento, fazer subtracao, caso contrario nao fazer operacao
    (Len_Lista_Pals > 1 ->                                                                                                                                                                                                          %  TENTAR IF-ELSE
        subtract(Lista_Pals, Pals_Unicas, Lista_Pals_act);
        Lista_Pals_act = Lista_Pals
    ).


%--------------------------------------------------------------------------------------------%
% simplifica(Pals_Possiveis, Pals_Possiveis) afirma que Pals_Possiveis e o resultado de      %
% simplificar Pals_Possiveis, isto e, aplicar os passos definidos pelos 3 ultimos predicados %
% ate nao haver alteracoes                                                                   %
%--------------------------------------------------------------------------------------------%

simplifica(Pals_Possiveis, Novas_Pals_Possiveis) :-

    atribui_comuns(Pals_Possiveis),                                          
    retira_impossiveis(Pals_Possiveis, Pals_sem_impossiveis),                 
    retira_unicas(Pals_sem_impossiveis, Pals_Possiveis_act),                
    
    % Se nao houver alteracoes, parar processo de simplificacao 
    (Pals_Possiveis_act \== Pals_Possiveis ->
        simplifica(Pals_Possiveis_act, Novas_Pals_Possiveis);
        Novas_Pals_Possiveis = Pals_Possiveis_act              
    ).


%----------------------------------------------------------------------------------%
% inicializa(Puz, Pals_Possiveis) afirma que Pals_Possiveis e a lista de palavras  %
% simplificada para o puzzle Puz                                                   %
%----------------------------------------------------------------------------------%

inicializa(Puz, Pals_Possiveis) :-
    
    nth1(1, Puz, Palavras),
    obtem_letras_palavras(Palavras, Letras),
    
    nth1(2, Puz, Grelha),
    espacos_puzzle(Grelha, Espacos),

    palavras_possiveis(Letras, Espacos, Lst_Pals),

    simplifica(Lst_Pals, Pals_Possiveis).


%----------------------------------------------------------------------------------------%
% escolhe_menos_alternativas(Pals_Possiveis, Escolha) afirma que Escolha e o elemento de %
% Pals_Possiveis com o menor numero de palavras associadas que figura em primeiro lugar  %
%----------------------------------------------------------------------------------------%

escolhe_menos_alternativas(Pals_Possiveis, Escolha) :-

    % Remover de Pals_Possiveis elementos associadas a uma so palavra 
    exclude(filtra_unicas, Pals_Possiveis, Pals_filtradas),

    % Assegurar que a lista resultante possui elementos 
    Pals_filtradas \== [],!,

    % Achar o elemento com menor numero de palavras nestas condicoes 
    menor_num_pals(Pals_filtradas, Escolha).

% menor_num_pals(Pals, Escolha) afirma que Escolha e o elemento de Pals com menor numero de palavras
menor_num_pals(Pals, Escolha) :-

    % provar versao iterativa estabelecendo o primeiro elemento como minimo 
    nth1(1, Pals, Esp_min_palavras, Resto),
    menor_num_pals(Resto, Esp_min_palavras, Escolha).

menor_num_pals([], Escolha, Escolha) :- !.

menor_num_pals([Palavra|Resto], Min, Escolha) :-

    % comparar numero de palavras de Min com o elemento da Lista a analisar
    nth1(2, Palavra, Lista_Pals),
    length(Lista_Pals, Num_Pals),

    nth1(2, Min, Lista_Min),
    length(Lista_Min, Num_Min),

    (Num_Pals < Num_Min ->  
        menor_num_pals(Resto, Palavra, Escolha);
        menor_num_pals(Resto, Min, Escolha)
    ).


%------------------------------------------------------------------------------------------------%
% experimenta_pal(Escolha, Pals_Possiveis, Novas_Pals_Possiveis) afirma que Novas_Pals_Possiveis %
% e o resultado de atribuir uma palavra de Escolha ao seu espaco e retirar as restantes palavras %
% associadas em Pals_Possiveis                                                                   %
%------------------------------------------------------------------------------------------------%

experimenta_pal(Escolha, Pals_Possiveis, Novas_Pals_Possiveis) :-

    % Obter espaco e lista de palavras de Escolha
    nth1(1, Escolha, Esp),
    nth1(2, Escolha, Lst_Pals),

    % Escolher uma palavra e unifica-la com o espaco 
    member(Pal, Lst_Pals),
    Pal = Esp,

    reduz_palavras(Escolha, Pals_Possiveis, Novas_Pals_Possiveis).

% Aplica reducao ao elemento Escolha de Pals_Possiveis
reduz_palavras(Escolha,[El|Resto], [El_act|Resto]) :-
    El == Escolha,
    !,
    nth1(1, Escolha, Esp),
    El_act=[Esp, [Esp]].

reduz_palavras(Escolha,[El|Resto], [El|Resto_act]) :-
    reduz_palavras(Escolha, Resto, Resto_act).


%--------------------------------------------------------------------------------------------%
% resolve_aux(Pals_Possiveis, Novas_Pals_Possiveis) afirma que Novas_Pals_Possiveis resulta  %
% de aplicar o algoritmo de resolucao de puzzles a Pals_Possiveis: fazer a escolha com menos %
% alternativas, experimentar uma palavra dessa escolha e simplificar as palavras possiveis   %
%--------------------------------------------------------------------------------------------%

resolve_aux(Pals_Possiveis, Novas_Pals_Possiveis) :-
    
    escolhe_menos_alternativas(Pals_Possiveis, Escolha),!,
    experimenta_pal(Escolha, Pals_Possiveis, Pals_Exp),
    simplifica(Pals_Exp, Pals_Possiveis_act),

    resolve_aux(Pals_Possiveis_act, Novas_Pals_Possiveis).

resolve_aux(Pals_Possiveis, Pals_Possiveis).
    

%------------------------------------------------------------------------%
% resolve(Puz) resolve Puz dando uma substituicao para as suas variaveis %
%------------------------------------------------------------------------%

resolve(Puz) :-
    inicializa(Puz, Pals_Possiveis),
    resolve_aux(Pals_Possiveis, _).