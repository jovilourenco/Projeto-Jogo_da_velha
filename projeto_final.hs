{-
    Aluno: João Victor Lourenço da Silva
    Matrícula: 20220005997
-}

-- imports

import Control.Exception
import System.IO.Error
import System.IO
import System.Process
import Data.List
import Data.Function

-- Tipos de dados que serão usados.

type Jogadores = [Jogador]
type Nome = String
type Pontuacao = Int
type Vez = Int
type Tabela = [Char]

data Jogador = Jogador Nome Pontuacao
                    deriving (Show, Read)

-- FUNÇÃO AUXILIAR
getString :: String -> IO String
getString str = do
                    putStr str
                    res <- getLine
                    return res

-- Função de inicialização: Abre o arquivo de ranking e chama o menu p/ escolha das opções.
inicio :: IO()
inicio = do
            {catch (ler_arquivo) tratar_erro;}
            where
                ler_arquivo = do { -- Função abstrata para leitura
                        arq <- openFile "rank.txt" ReadMode;
                        dados <- hGetLine arq;
                        hClose arq;
                        menu (read dados);
                        return()
                    }
                tratar_erro erro = if isDoesNotExistError erro then do{ -- Função abstrata para tratamento de erro de leitura
                    arq <- openFile "rank.txt" WriteMode;
                    hPutStrLn arq "[]";
                    hClose arq;
                    menu [];
                    return()
                }
                else
                    ioError erro

-- Função de exibição do menu (função principal)
menu :: Jogadores -> IO Jogadores
menu dados = do
                system "cls" -- Limpar tela (somente para windows)
                putStrLn "---------- Jogo da Velha ----------"
                putStrLn "\nDigite 1 para cadastrar jogador;"
                putStrLn "Digite 2 para jogar;"
                putStrLn "Digite 3 para visualizar o ranking;"
                putStrLn "Digite 0 para sair."
                putStr "Opção: "
                op <- getChar
                getChar -- Ignorar o Enter
                executarOpcao dados op
                
-- Função para tratar a escolha da opção escolhida
executarOpcao :: Jogadores -> Char -> IO Jogadores
executarOpcao dados '1' = cadastrarJogador dados
executarOpcao dados '2' = prepararJogo dados
executarOpcao dados '3' = do
                            system "cls"
                            putStrLn "\nRanking dos jogadores:\n"
                            if null dados then do
                                putStrLn "Não há jogadores cadastrados!"
                                putStr "\nPressione <Enter> para voltar ao menu."
                                getChar
                                menu dados
                            else do
                                exibirRanking (reverse (ordenar dados))
                                putStr "\nPressione <Enter> para voltar ao menu."
                                getChar
                                menu dados
executarOpcao dados '0' = do
                            putStrLn ("\nObrigado por jogar!")
                            return dados
executarOpcao dados _ = do
                            putStrLn("\nOpção inválida! Pressione <Enter> para voltar...")
                            getChar
                            menu dados

-- Função de cadastro de jogadores
cadastrarJogador :: Jogadores -> IO Jogadores
cadastrarJogador dados = do
                            system "cls"
                            nome <- getString "\nDigite um nome de usuário: "
                            if(existeJogador dados nome) then do
                                putStrLn "\nEsse nome já existe, tente outro. <Enter> para continuar."
                                getChar
                                menu dados
                            else do
                                arq <- openFile "rank.txt" WriteMode
                                hPutStrLn arq (show ((Jogador nome 0): dados))
                                hClose arq
                                putStrLn ("\nUsuário cadastrado com sucesso! <Enter> para continuar.")
                                getChar
                                menu ((Jogador nome 0): dados)

-- Verificar se jogador existe
existeJogador :: Jogadores -> Nome -> Bool
existeJogador [] _ = False
existeJogador ((Jogador n p):xs) nome
                    | (n == nome) = True
                    | otherwise = existeJogador xs nome

-- Função que prepara o início do jogo
prepararJogo :: Jogadores -> IO Jogadores
prepararJogo dados = do
                    jogador1 <- getString "\nDigite o nome do primeiro jogador: "
                    if not (existeJogador dados jogador1) then do
                        putStrLn "\nEsse jogador não existe! Pressione <Enter> para voltar..."
                        getChar
                        menu dados
                    else do
                        jogador2 <- getString "\nDigite o nome do segundo jogador: "
                        if not (existeJogador dados jogador2) then do
                            putStrLn "\nEsse jogador não existe! Pressione <Enter> para voltar..."
                            getChar
                            menu dados
                        else do
                            novoJogo dados jogador1 jogador2

-- Função que inicia um novo jogo
novoJogo :: Jogadores -> Nome -> Nome -> IO Jogadores
novoJogo dados jogador1 jogador2 = do
                                        system "cls"
                                        putStrLn ("\nIniciando... \"" ++
                                                    jogador1 ++ " vs " ++ jogador2 ++ "\" ... ")
                                        putStrLn("\nOs quadrados que possuem números NÃO estão marcados.")
                                        putStrLn ("\n" ++ jogador1 ++ " será o \'X\' e " ++ jogador2 ++ " será o \'O\'. Vamos lá!!")
                                        
                                        {-
                                        Configuração inicial em formato de lista: [1,2,3,4,5,6,7,8,9]
                                        Representação:
                                                        1 | 2 | 3
                                                       -----------
                                                        4 | 5 | 6
                                                       -----------
                                                        7 | 8 | 9
                                        -}

                                        -- Passa os dados, a configuração inicial, os jogadores e uma flag que indica de quem é
                                        -- a vez: 0 é o jogador1 e 1 é o jogador2
                                        rodarJogo dados ['1','2','3','4','5','6','7','8','9'] jogador1 jogador2 0

-- Função que exibe o tabuleiro do jogo da velha e implementa a lógica baseada em verificações
rodarJogo :: Jogadores -> Tabela -> Nome -> Nome -> Vez -> IO Jogadores
rodarJogo dados tabela jogador1 jogador2 vez = do
    putStrLn $ "\n" ++
               "                              " ++ show (tabela !! 0) ++ " | " ++ show (tabela !! 1) ++ " | " ++ show (tabela !! 2) ++ "\n" ++
               "                              ---------------\n" ++
               "                              " ++ show (tabela !! 3) ++ " | " ++ show (tabela !! 4) ++ " | " ++ show (tabela !! 5) ++ "\n" ++
               "                              ---------------\n" ++
               "                              " ++ show (tabela !! 6) ++ " | " ++ show (tabela !! 7) ++ " | " ++ show (tabela !! 8) ++ "\n"
    if (venceuJogador1 tabela) then do
        putStrLn ("Parabéns " ++ jogador1 ++ "! Você venceu!!")

        arq_escrita <- openFile "rank.txt" WriteMode
        hPutStrLn arq_escrita (show (atualizaPontuacao dados jogador1))
        hClose arq_escrita

        arq_leitura <- openFile "rank.txt" ReadMode
        dados_atualizados <- hGetLine arq_leitura
        hClose arq_leitura

        putStr "\nPressione <Enter> para voltar ao menu..."
        getChar
        menu (read dados_atualizados)
    else do
        if (venceuJogador2 tabela) then do
            putStrLn ("Parabéns " ++ jogador2 ++ "! Você venceu!!")
    
            arq_escrita <- openFile "rank.txt" WriteMode
            hPutStrLn arq_escrita (show (atualizaPontuacao dados jogador2))
            hClose arq_escrita
    
            arq_leitura <- openFile "rank.txt" ReadMode
            dados_atualizados <- hGetLine arq_leitura
            hClose arq_leitura
    
            putStr "\nPressione <Enter> para voltar ao menu..."
            getChar
            menu (read dados_atualizados)
        else do
            -- se o tamanho da intersecção entre "123456789" e "tabela" for 0, então não há mais jogadas, deu empate.
            if((length (intersect "123456789" tabela)) == 0) then do
                putStrLn ("Deu empate!")
                putStr "\nPressione <Enter> para voltar ao menu..."
                getChar
                menu dados
            else do
                if (vez == 0) then do
                    putStr (jogador1 ++ ", é a sua vez! onde quer marcar? ")
                    op <- getChar
                    getChar
                    if not (elem op "123456789") then do
                        putStrLn "\nEssa opção não é válida. Tente novamente"
                        rodarJogo dados tabela jogador1 jogador2 0
                    else 
                        if not (elem op tabela) then do
                            putStrLn "\nEssa opção já foi marcada, escolha outra."
                            rodarJogo dados tabela jogador1 jogador2 0
                        else 
                            rodarJogo dados (obterNovoTabuleiro tabela vez op) jogador1 jogador2 1
                else do
                    putStr (jogador2 ++ ", é a sua vez! onde quer marcar? ")
                    op <- getChar
                    getChar
                    if not (elem op "123456789") then do
                        putStrLn "\nEssa opção não é válida. Tente novamente "
                        rodarJogo dados tabela jogador1 jogador2 1
                    else 
                        if not (elem op tabela) then do
                            putStrLn "\nEssa opção já foi marcada, escolha outra. "
                            rodarJogo dados tabela jogador1 jogador2 1
                        else 
                            rodarJogo dados (obterNovoTabuleiro tabela vez op) jogador1 jogador2 0

-- Função que recebe uma lista com a configuração atual do tabuleiro, a vez, uma opção e retorna uma nova configuração
obterNovoTabuleiro :: Tabela -> Vez -> Char -> Tabela
obterNovoTabuleiro (x:xs) vez e
                            | ((x == e) && (vez == 0)) = (['X'] ++ xs)
                            | ((x == e) && (vez == 1)) = (['O'] ++ xs)
                            | otherwise = x:(obterNovoTabuleiro xs vez e)

-- Função que verifica se o jogador1 venceu.
venceuJogador1 :: Tabela -> Bool
venceuJogador1 tabela
                -- Verifica linhas
                | (((tabela !! 0) == 'X') && ((tabela !! 1) == 'X') && ((tabela !! 2) == 'X')) = True
                | (((tabela !! 3) == 'X') && ((tabela !! 4) == 'X') && ((tabela !! 5) == 'X')) = True
                | (((tabela !! 6) == 'X') && ((tabela !! 7) == 'X') && ((tabela !! 8) == 'X')) = True
                -- Verifica colunas
                | (((tabela !! 0) == 'X') && ((tabela !! 3) == 'X') && ((tabela !! 6) == 'X')) = True
                | (((tabela !! 1) == 'X') && ((tabela !! 4) == 'X') && ((tabela !! 7) == 'X')) = True
                | (((tabela !! 2) == 'X') && ((tabela !! 5) == 'X') && ((tabela !! 8) == 'X')) = True
                -- Verifica diagonais
                | (((tabela !! 0) == 'X') && ((tabela !! 4) == 'X') && ((tabela !! 8) == 'X')) = True
                | (((tabela !! 2) == 'X') && ((tabela !! 4) == 'X') && ((tabela !! 6) == 'X')) = True
                | otherwise = False

-- Função que verifica se o jogador2 venceu.
venceuJogador2 :: Tabela -> Bool
venceuJogador2 tabela
                -- Verifica linhas
                | (((tabela !! 0) == 'O') && ((tabela !! 1) == 'O') && ((tabela !! 2) == 'O')) = True
                | (((tabela !! 3) == 'O') && ((tabela !! 4) == 'O') && ((tabela !! 5) == 'O')) = True
                | (((tabela !! 6) == 'O') && ((tabela !! 7) == 'O') && ((tabela !! 8) == 'O')) = True
                -- Verifica colunas
                | (((tabela !! 0) == 'O') && ((tabela !! 3) == 'O') && ((tabela !! 6) == 'O')) = True
                | (((tabela !! 1) == 'O') && ((tabela !! 4) == 'O') && ((tabela !! 7) == 'O')) = True
                | (((tabela !! 2) == 'O') && ((tabela !! 5) == 'O') && ((tabela !! 8) == 'O')) = True
                -- Verifica diagonais
                | (((tabela !! 0) == 'O') && ((tabela !! 4) == 'O') && ((tabela !! 8) == 'O')) = True
                | (((tabela !! 2) == 'O') && ((tabela !! 4) == 'O') && ((tabela !! 6) == 'O')) = True
                | otherwise = False

-- Função que atualiza pontuação do vencedor
atualizaPontuacao :: Jogadores -> String -> Jogadores
atualizaPontuacao ((Jogador nome pontuacao):xs) vencedor
                | (nome == vencedor) = [(Jogador nome (pontuacao + 1))] ++ xs
                | otherwise = (Jogador nome pontuacao):(atualizaPontuacao xs vencedor)

-- Função para exibir o ranking dos jogadores
exibirRanking :: Jogadores -> IO ()
exibirRanking [] = return ()
exibirRanking (x:xs) = do
                            putStrLn ((obterNome x) ++ " possui " ++ (show (obterPontuacao x)) ++ " pontos.")
                            exibirRanking xs

-- Função auxiliar para retornar apenas o nome do jogador
obterNome :: Jogador -> Nome
obterNome (Jogador nome _) = nome

-- Função auxiliar para retornar apenas a pontuação do jogador
obterPontuacao :: Jogador -> Pontuacao
obterPontuacao (Jogador _ pontuacao) = pontuacao

-- Função que ordena os jogadores com base na pontuação (crescente).
ordenar :: Jogadores -> Jogadores
ordenar dados = sortBy (compare `on` obterPontuacao) dados