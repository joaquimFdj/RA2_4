import qualified Data.Map as Map
import Data.Time (UTCTime, getCurrentTime)
import System.IO (openFile, hPutStrLn, hClose, hFlush, stdout, IOMode(AppendMode))
import System.Directory (doesFileExist)

data Item = Item {
    itemID :: String,
    nome :: String,
    quantidade :: Int,
    categoria :: String
} deriving (Show, Read)

type Inventario = Map.Map String Item

data AcaoLog = Add | Remove | Update | Query | QueryFail | Delete
    deriving (Show, Read, Eq)
    
data StatusLog = Sucesso | Falha String
    deriving (Show, Read, Eq)
    
data LogEntry = LogEntry {
    timestamp :: UTCTime,
    acao :: AcaoLog,
    detalhes :: String,
    status :: StatusLog
} deriving (Show, Read)
        
type ResultadoOperacao = (Inventario, LogEntry)
    
-- ####################### FUNÇÕES PURAS DE NEGOCIO ############################

addItem :: UTCTime -> Item -> Inventario -> Either (String, LogEntry) ResultadoOperacao
addItem horario item inventario
    | Map.member (itemID item) inventario =
        let
            erro = "O item " ++ itemID item ++ " ja esta registrado no inventario"
            logErro = LogEntry horario Add ("Falha: " ++ erro) (Falha erro)
        in Left (erro, logErro)
    
    | otherwise =
        let
            novoInv = Map.insert (itemID item) item inventario
            logSucesso = LogEntry horario Add ("Item adicionado: " ++ nome item) Sucesso
        in Right (novoInv, logSucesso)
        

removeItem :: UTCTime -> String -> Int -> Inventario -> Either (String, LogEntry) ResultadoOperacao
removeItem horario codigoItem quant inventario 
    | not (Map.member codigoItem inventario) =
        let
            erro = "Item " ++ codigoItem ++ " nao encontrado"
            logNaoEncontrado = LogEntry horario Remove ("Falha: " ++ erro) (Falha erro)
        in Left (erro, logNaoEncontrado)
    
    | quant > quantAtual =
        let
            erro = "Estoque insuficiente para remover essa quantidade!"
            logExcesso = LogEntry horario Remove ("Falha: " ++ erro) (Falha erro)
        in Left (erro, logExcesso)
    
    | otherwise =
        let
            novaQuant = quantAtual - quant
            novoInv = Map.adjust (\i -> i {quantidade = novaQuant}) codigoItem inventario
            logRemoveu = LogEntry horario Remove ("Removido " ++ show quant ++ " unidades do item " ++ codigoItem ++ 
                        ". Estoque anterior: " ++ show quantAtual ++ 
                        ", estoque atual: " ++ show novaQuant) Sucesso
        in Right(novoInv, logRemoveu)
            
    where 
        quantAtual = case Map.lookup codigoItem inventario of
            Just item -> quantidade item
            Nothing -> 0
            
            
consultarInventario :: UTCTime -> Inventario -> Either (String, LogEntry) ([String], LogEntry)
consultarInventario horario inventario
    | Map.null inventario =
        let
            erro = "Inventario esta vazio"
            logVazio = LogEntry horario QueryFail ("Falha: " ++ erro) (Falha erro)
        in Left(erro, logVazio)
    | otherwise =
        let 
            stringsInventario = map (\item -> "ID: " ++ itemID item ++
                                     ", Nome: " ++ nome item ++
                                     ", Quantidade: " ++ show (quantidade item) ++
                                     ", Categoria: " ++ categoria item) (Map.elems inventario)
            logSucesso = LogEntry horario Query ("Inventario consultado com sucesso") Sucesso
        in Right(stringsInventario, logSucesso)


updateQty :: UTCTime -> Inventario -> String -> Int -> Either (String, LogEntry) ResultadoOperacao
updateQty horario inventario codigoItem novaQuant
    | not (Map.member codigoItem inventario) =
        let
            erro = "Item " ++ codigoItem ++ " nao encontrado"
            logNaoEncontrado = LogEntry horario Update ("Falha: " ++ erro) (Falha erro)
        in Left (erro, logNaoEncontrado)
    
    | novaQuant < 0 =
        let
            erro = "Quantidade " ++ show novaQuant ++ " invalida!"
            logQuantInvalida = LogEntry horario Update ("Falha: " ++ erro) (Falha erro)
        in Left(erro, logQuantInvalida)
    
    | otherwise =
        let
            novoInv = Map.adjust (\i -> i {quantidade = novaQuant}) codigoItem inventario 
            logUpdate = LogEntry horario Update ("Quantidade do item " ++ codigoItem ++ " alterada para: " ++ show novaQuant) Sucesso
        in Right(novoInv, logUpdate)
        
        
deletaItem :: UTCTime -> Inventario -> String -> Either (String, LogEntry) ResultadoOperacao
deletaItem horario inventario codigoItem 
    | not (Map.member codigoItem inventario) =
        let 
            erro = "Item " ++ codigoItem ++ " nao encontrado"
            logNaoEncontrado =  LogEntry horario Delete ("Falha: " ++ erro) (Falha erro)
        in Left(erro, logNaoEncontrado)
    
    | otherwise =
        let 
            novoInv = Map.delete codigoItem inventario
            logDeletou = LogEntry horario Delete ("Item " ++ codigoItem ++ " foi deletado!") Sucesso
        in Right(novoInv, logDeletou)

-- #############################################################################

-- ############ FUNÇÕES DE I/O - ENVOLVEM EFEITOS COLATERAIS ###################

-- IO do arquivo Inventario.dat:
carregarInventario :: FilePath -> IO Inventario
carregarInventario caminho = do
    conteudo <- readFile caminho
    let linhas = lines conteudo 
        itens = lerItens linhas
        inventario = Map.fromList [(itemID i, i) | i <- itens]
    return inventario

lerItens :: [String] -> [Item]
lerItens [] = []
lerItens (idItem:nome:quantidade:categoria:resto) =
    let item = Item idItem nome (read quantidade) categoria
    in item : lerItens resto
lerItens _ = []

salvarInventario :: FilePath -> Inventario -> IO ()
salvarInventario caminho inventario = do
    let itens = Map.elems inventario
        linhas = concatMap itemParaLinhas itens
        conteudo = unlines linhas
    writeFile caminho conteudo
    hFlush stdout

itemParaLinhas :: Item -> [String]
itemParaLinhas item =
    [ itemID item
    , nome item
    , show (quantidade item)
    , categoria item
    ]
    
-- IO do arquivo Auditoria.log
carregarLogs :: FilePath -> IO [LogEntry]
carregarLogs caminho = do
    conteudo <- readFile caminho
    let linhas = lines conteudo
        logs = lerLogs linhas 
    return logs

lerLogs :: [String] -> [LogEntry]
lerLogs [] = []
lerLogs (horarioString:acaoString:detalhes:statusString:resto) = 
    let 
        horario = read horarioString :: UTCTime
        acao = read acaoString :: AcaoLog
        status = if statusString == "Sucesso"
                then Sucesso
                else Falha (drop 6 statusString) 
    in LogEntry horario acao detalhes status : lerLogs resto
lerLogs _ = []

salvarLog :: LogEntry -> IO ()
salvarLog logSalvar = do
    
    arquivo <- openFile "Auditoria.log" AppendMode
    hPutStrLn arquivo (unlines (logParaLinhas logSalvar))
    hClose arquivo
    hFlush stdout

logParaLinhas :: LogEntry -> [String]
logParaLinhas logSalvar =
    [ show (timestamp logSalvar)
    , show (acao logSalvar)
    , detalhes logSalvar
    , case status logSalvar of 
        Sucesso -> "Sucesso"
        Falha msg -> "Falha: " ++ msg
    ]

-- #############################################################################

-- ### CAMINHOS DOS ARQUIVOS ###

arquivoInv :: FilePath
arquivoInv = "Inventario.dat"

arquivoLog :: FilePath
arquivoLog = "Auditoria.log"

-- #############################

-- ########################### FUNCOES DA  MAIN ################################

loopPrincipal :: Inventario -> IO ()
loopPrincipal inventario = do
    putStrLn "-SISTEMA DE INVENTARIO-"
    putStrLn "ESCOLHA UMA OPCAO DE OPERACAO"
    putStrLn "1- Adicionar Item ao Inventario"
    putStrLn "2- Remover Quantidade de um Item"
    putStrLn "3- Definir Quantidade de um Item"
    putStrLn "4- Deletar Item do Inventario"
    putStrLn "5- Exibir Inventario"
    putStrLn "report- Relatorio Sobre os Logs"
    putStrLn "exit- SAIR DO SISTEMA"
    putStr "Opcao selecionada: "
    hFlush stdout
    opcao <- getLine
    
    case opcao of
        "1" -> do
            novoInv <- acaoUm inventario
            loopPrincipal novoInv
            
        "2" -> do
            novoInv <- acaoDois inventario
            loopPrincipal novoInv
            
        "3" -> do
            novoInv <- acaoTres inventario
            loopPrincipal novoInv
            
        "4" -> do
            novoInv <- acaoQuatro inventario
            loopPrincipal novoInv
            
        "5" -> do
            acaoCinco inventario
            loopPrincipal inventario
            
        "report" -> do
            novoInv <- acaoCinco inventario
            loopPrincipal inventario
            
        "exit" -> do
            putStrLn "SALVANDO E FECHANDO SISTEMA"
            salvarInventario arquivoInv inventario
            return ()
            
        _ -> do
            putStrLn "ENTRADA INVALIDA: ESCOLHA OUTRA OPCAO"
            loopPrincipal inventario
            
acaoUm :: Inventario -> IO Inventario
acaoUm inventario = do
    putStrLn "Insira o ID do Item a ser adicionado: "
    hFlush stdout
    codigoItem <- getLine
    putStrLn "Insira o nome do Item a ser adicionado: "
    hFlush stdout
    nomeItem <- getLine
    putStrLn "Insira a quantidade inicial do Item a ser adicionado: "
    hFlush stdout
    quantidadeString <- getLine
    let quantidadeItem = read quantidadeString
    putStrLn "Digite a categoria do Item a ser adicionado: "
    hFlush stdout
    categoriaItem <- getLine
    
    horario <- getCurrentTime
    let item = Item codigoItem nomeItem quantidadeItem categoriaItem
    case addItem horario item inventario of
        Left (_, logErro) -> do
            salvarLog logErro
            putStrLn "Erro na Adicao do Item: Log de Erro salvo."
            return inventario
        Right (novoInv, logSucesso) -> do
            salvarLog logSucesso
            salvarInventario arquivoInv novoInv
            putStrLn "Item adicionado e salvo com sucesso!"
            return novoInv
            
acaoDois :: Inventario -> IO Inventario
acaoDois inventario = do
    putStrLn "Insira o ID do Item a ter seu Estoque subtraido: "
    hFlush stdout
    codigoItem <- getLine
    putStrLn "Insira a quantidade a ser subtraida: "
    hFlush stdout
    quantidadeString <- getLine
    let quantidadeItem = read quantidadeString
    
    horario <- getCurrentTime
    case removeItem horario codigoItem quantidadeString inventario of
        Left (_, logErro) -> do
            salvarLog logErro
            putStrLn "Erro na remocao do Item: Log de Erro salvo."
            return inventario
        Right (novoInv, logSucesso) -> do
            salvarLog logSucesso
            salvarInventario arquivoInv novoInv
            putStrLn "Quantidade subtraida e salva com sucesso!"
            return novoInv
            
acaoTres :: Inventario -> IO Inventario
acaoTres inventario = do
    putStrLn "Insira o ID do Item a ter seu Estoque atualizado: "
    hFlush stdout
    codigoItem <- getLine
    putStrLn "Insira a quantidade a ser atribuida: "
    hFlush stdout
    quantidadeString <- getLine
    let quantidadeItem = read quantidadeString
    
    horario <- getCurrentTime
    case updateQty horario inventario codigoItem quantidadeItem of
        Left (_, logErro) -> do
            salvarLog logErro
            putStrLn "Erro na atribuicao de uma quantidade: Log de Erro salvo."
            return inventario
        Right (novoInv, logSucesso) -> do
            salvarLog logSucesso
            salvarInventario arquivoInv novoInv
            putStrLn "Estoque atribuido e salvo com sucesso!"
            return novoInv
            
acaoQuatro :: Inventario -> IO Inventario
acaoQuatro inventario = do
    putStrLn "Insira o ID do Item a ser deletado: "
    hFlush stdout
    codigoItem <- getLine
    
    horario <- getCurrentTime
    case deletaItem horario inventario codigoItem of
        Left (_, logErro) -> do
            salvarLog logErro
            putStrLn "Erro ao deletar item: Log de Erro salvo."
        Right (novoInv, logSucesso) -> do
            salvarLog logSucesso
            salvarInventario arquivoInv novoInv
            putStrLn "Item deletado e salvo com sucesso!"
            return novoInv
            
acaoCinco :: Inventario -> IO()
acaoCinco inventario = do
    horario <- getCurrentTime
    case consultarInventario horario inventario of
        Left (_, logErro) -> do
            salvarLog logErro
            putStrLn "Erro na consulta de inventario: log de erro salvo"
        Right (consulta, logSucesso) -> do
            salvarLog logSucesso
            imprimeConsulta consulta
    where
        imprimeConsulta :: [String] -> IO()
        imprimeConsulta [] = return ()
        imprimeConsulta (consulta:resto) = do
            print consulta
            imprimeConsulta resto
        
    
    

main :: IO ()
main = do
    putStrLn "=== Sistema de Inventário ==="

    existeInv <- doesFileExist arquivoInv
    existeLog <- doesFileExist arquivoLog

    inventario <- if existeInv
        then carregarInventario arquivoInv
        else return Map.empty

    loopPrincipal inventario
            
-- #############################################################################






