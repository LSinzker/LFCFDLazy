\documentclass[12pt]{article}

%include polycode.fmt

\usepackage[utf8]{inputenc}
\usepackage[brazil]{babel}
\usepackage{fontenc}
\usepackage{biblatex}
\usepackage{csquotes}
\usepackage[nottoc]{tocbibind}
\addbibresource{references.bib}

\title{Implementa\c c\~{a}o de Recur\c c\~{a}o e avalia\c c\~{a}o \textit{Lazy} em Haskell}

\author{Luisa Sinzker Fantin MATR\'{I}CULA\\
        Jo\~{a}o Pedro Silva Sousa, 15/0038381\\
        Rafael Oliveira de Souza
}

\begin{document}

\maketitle

\section{Introdu\c c\~{a}o}
\label{introduction}

Dentro do universo das linguagens de programa\c c\~{a}o é poss\'{i}vel
dividi-las em várias classifica\c c\~{o}es diferentes, seja de acordo com
o paradigma de programa\c c\~{a}o utilizado por essa linguagem (como funcional
e procedural), com rela\c c\~{a}o aos tipos (forte ou fracamente tipada,
estática ou dinamicamente tipada), às estratégias de avalia\c c\~{a}o utilizada
na linguagem (\textit{eager} ou \textit{lazy}) e diversas outras formas.


\subsection{Introdu\c c\~{a}o te\'{o}rica}
\label{sec:theory}

Dos mecanismos (ordem) de avalia\c c\~{a}o de uma linguagem de programa\c c\~{a}o,
a estrat\'{e}gia \textit{lazy} \'{e} bastante conhecida e utilizada na
linguagem Haskell. A avalia\c c\~{a}o \textit{lazy}, na aus\^{e}ncia de efeitos
colaterais, possui semântica muito parecida com uma avalia\c c\~{a}o na ordem
convencional.

Basicamente, a estrat\'{e}gia \textit{lazy} \'{e} uma fus\~{a}o entre
m\'{e}todos de avalia\c c\~{a}o não rigorosos (\textit{non-strict}) com
um mecanismo que evita a avalia\c c\~{a} repetida de express\~{o}es as quais
j\'{a} se conhece o resultado (chamado de \textit{sharing}).

Uma linguagem \'{e} dita \textit{strict} se todas as fun\c c\~{o}es s\~{a}o
sempre estritas, ou seja, uma fun\c c\~{a}o s\'{o} ser\'{a} definida se todos os
seus argumentos s\~{a}o conhecidos (avaliados). Uma linguagem n\~{a}o-estrita
n\~{a}o possui esse requisito, ou seja, podem existir fun\c c\~{o}es definidas
mesmo que nem todos os seus argumentos s\~{a}o conhecidos \cite{Scott}.

O mecanismo \textit{sharing} baseia-se na constru\c c\~{a}o de uma tabela, que
mapeia cada argumento com a respectiva express\~{a} j\'{a} avaliada.


\subsection{Contextualiza\c c\~{a}o}
\label{sec:context}

O foco deste trabalho consiste na implementa\c c\~{a}o de um interpretador que
possua estrat\'{e}gia \textit{lazy} de avalia\c c\~{a}o, al\'{e}m de suporte
a chamadas de fun\c c\~{o}es recursivas.


\section{Vis\~{a}o geral da linguagem}
\label{sec:lang}

A linguagem LFCFDLazy fornecida possuia suporte a express\~{o}es identificadas
(LET), refer\^{e}ncias \`{a} identificadores e fun\c c\~{o}es de alta ordem
e suas avalia\c c\~{o}es (mecanismo de express\~{o}es lambda).

Para que seja poss\'{i}vel alcan\c c\~{a}r o objetivo, foram adcionados ao
interpretador da linguagem suporte \`{a} avalia\c c\~{o}es de condicionais
(necess\'{a}rias para a implementa\c c\~{a}o da recurs\~{a}o) e a pr\'{o}pria
recurs\~{a}o \cite{Shriram}.


\section{Estudo do interpretador}
\label{sec:interp_stdy}

\begin{code}
    module LFCFDLazy where
\end{code}

\subsection{Defni\c c\~{a}o dos tipos e estruturas}
\label{sec:type_data}

\begin{code}

    type Id = String

    type Env = [(Id, ValorE)]

    type DefredSub = [(Id, Expressao)]


    data ValorE = VInt Int
                | FClosure Id Expressao Env
                | EClosure Expressao Env
        deriving (Show, Eq)

    data Expressao = Valor Int
                   | Soma Expressao Expressao
                   | Subtracao Expressao Expressao
                   | Multiplicacao Expressao Expressao
                   | Divisao Expressao Expressao
                   | Let Id Expressao Expressao
                   | Ref Id
                   | Lambda Id Expressao
                   | Aplicacao Expressao Expressao
                   | If0 Expressao Expressao Expressao
                   | Rec Id Expressao Expressao
        deriving (Show, Eq)

\end{code}

O tipo \emph{Id} \'{e} apenas uma renomea\c c\~{a}o para um identificador, que
pode ser de uma vari\'{a}vel ou de uma fun\c c\~{a}o recursiva. O tipo \emph{Env}
\'{e} o ambiente de mapeamento (tupla) entre os identificadores e suas
respectivas express\~{o}es associadas. O tipo \emph{DefredSub} \'{e} o ambiente
de substitui\c c\~{o}es postergadas que, analogamente mapeiam identificadores
\`{a} express\~{o}es por\'{e}m, com a diferen\c ca que elas ainda n\~{a}o
foram avaliadas.


\subsection{Fun\c c\~{o}es de pesquisa}
\label{sec:fun_search}

\begin{code}
    
    pesquisar :: Id -> Env -> ValorE
    pesquisar v [] = error "Variavel nao declarada."
    pesquisar v ((i,e):xs)
        | v == i = e
        | otherwise = pesquisar v xs


    searchApp :: Id -> ValorE -> Env -> Env
    searchApp n v [] = [(n, v)]
    searchApp n v ((i,e):xs)
        | n == i = []
        | otherwise = searchApp n v xs

\end{code}

As fun\c c\~{o}es \emph{pesquisar :: Id $\rightarrow$ Env $\rightarrow$ ValorE} e
\emph{searchApp :: Id $\rightarrow$ ValorE $\rightarrow$ Env $\rightarrow$ Env}
servem para procurar express\~{o}es mapeadas nos ambientes de substitui\c c\~{a}o
de identificadores e fun\c c\~{o}es recursivas, respectivamente.


\subsection{Fun\c c\~{o}es auxiliares}
\label{sec:fun_aux}

\begin{code}
    
    avaliacaoStrict :: ValorE -> ValorE
    avaliacaoStrict (EClosure e env) = avaliacaoStrict (avaliar e env)
    avaliacaoStrict e = e


    avaliarExpBin :: Expressao -> Expressao -> (Int -> Int -> Int) -> Env -> ValorE
    avaliarExpBin e d op env = VInt (op ve vd)
        where
            (VInt ve) = avaliacaoStrict (avaliar e env)
            (VInt vd) = avaliacaoStrict (avaliar d env')
            env' = case e of
                (Ref v) -> ((v, VInt ve):env)
                otherwise -> env

\end{code}

A fun\c c\~{a}o \emph{avaliacaoStrict :: ValorE $\rightarrow$ ValorE} realiza uma
avalia\c c\~{a}o de um \emph{EClosure} (closure de uma express\~{a}o). Caso o
\textit{closure} a ser avaliado por essa fun\c c\~{a}o seja um \textit{closure}
de uma fun\c c\~{a}o ou de um valor inteiro, a fun\c c\~{a} simplesmente retorna a
pr\'{o}pria express\~{a}o.

A fun\c c\~{a}o \emph{avaliarExpBin :: Expressao $\rightarrow$ Expressao $\rightarrow$ (Int $\rightarrow$ Int $\rightarrow$ Int) $\rightarrow$ Env $\rightarrow$ ValorE}
\'{e} utilizada para realizar a avalia\c c\~{a}o \textit{lazy} das expressoes
aritm\'{e}ticas bin\'{a}rias (adi\c c\~{a}o, subtra\c c\~{a}o, multiplica\c c\~{a}o
e divis\~{a}o). A fun\c c\~{a}o recebe como argumentos as duas express\~{o}es
que se deseja calcular, o operador e o ambiente de mapeamento de identificadores
e express\~{o}es j\'{a} avaliadas.

O m\'{e}todo de avalia\c c\~{a}o \textit{sharing} \'{e} implementado na senten\c c\~{a}
\emph{case} dessa fun\c c\~{a}o: caso a express\~{a}o disposta do lado direito
da opera\c c\~{a}o seja igual \`{a} express\~{a}o do lado esquerdo, ela n\~{a}o
\'{e} avaliada uma segunda vez, \'{e} realizada a recupera\c c\~{a}o da avalia\c c\~{a}o
da express\~{a}o do lado direto no ambiente \cite{Dolstra&Visser}.


\subsection{Interpretador}
\label{sec:interpreter}

\begin{code}

avaliar :: Expressao -> Env -> ValorE
avaliar (Valor n)             _ = VInt n
avaliar (Soma e d)          env = avaliarExpBin e d (+) env
avaliar (Subtracao e d)     env = avaliarExpBin e d (-) env
avaliar (Multiplicacao e d) env = avaliarExpBin e d (*) env
avaliar (Divisao e d)       env = avaliarExpBin e d div env
avaliar (Let v e c)         env = avaliar (Aplicacao (Lambda v c) e) env
avaliar (Ref v)             env = avaliacaoStrict (pesquisar v env)
avaliar (Lambda a c)        env = FClosure a c env
avaliar (Aplicacao e1 e2)   env =
    let
        v = avaliacaoStrict (avaliar e1 env)
        e = EClosure e2 env
    in case v of
        (FClosure a c env') -> avaliar c ((a, e):env')
        otherwise -> error "Tentando aplicar uma expressao" ++
                           "que nao eh uma funcao anonima"

avaliar (If0 v e d)         env
    | avaliar v env == VInt 0 = avaliar e env
    | otherwise = avaliar d env

avaliar (Rec nome e1 e2) env =
    let
        v = avaliacaoStrict (avaliar e1 env)
        e = EClosure e2 env
        env2 = (searchApp nome v env)++env
    in case v of
        (FClosure a c env') -> avaliar c ((a, e):env2)
        otherwise -> error "Tentando aplicar uma expressao" ++
                           "que nao eh uma funcao anonima"

\end{code}

Esse \'{e} o interpretador implementado, j\'{a} com as adi\c c\~{o}es requeridas
e necess\'{a}rias para o correto funcionamento da estrat\'{e}gia de
avalia\c c\~{a}o \textit{lazy} e suporte a chamadas de fun\c c\~{o}es recursivas.

As principais modifica\c c\~{o}es relativas \`{a} estrat\'{e}gia de avalia\c c\~{a}o
\textit{lazy} foram feitas na fun\c c\~{a}o \emph{avaliarExpBin}, j\'{a} que a
maioria das express\~{o}es s\~{a}o reduzidas \`{a}s opera\c c\~{o}es b\'{a}sicas
da aritm\'{e}tica, conforme detalhado na se\c c\~{a}o \ref{sec:fun_aux}.


\subsection{this}



\printbibliography


\end{document}