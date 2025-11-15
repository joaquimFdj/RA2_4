import qualified Data.Map as Map
import Data.Time (UTCTime, getCurrentTime)
import System.IO (openFile, hPutStr, hClose, hFlush, stdout, IOMode(AppendMode))
import System.Directory (doesFileExist)
import Text.Read (readMaybe)

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
            logErro = LogEntry horario Add ("Erro ao inserir item: " ++ itemID item) (Falha erro)
        in Left (erro, logErro)
    
    | otherwise =
        let
            novoInv = Map.insert (itemID item) item inventario
            logSucesso = LogEntry horario Add ("Item adicionado: " ++ itemID item) Sucesso
        in Right (novoInv, logSucesso)
        

removeItem :: UTCTime -> String -> Int -> Inventario -> Either (String, LogEntry) ResultadoOperacao
removeItem horario codigoItem quant inventario 
    | not (Map.member codigoItem inventario) =
        let
            erro = "Item " ++ codigoItem ++ " nao encontrado"
            logNaoEncontrado = LogEntry horario Remove ("Erro ao remover estoque do item: " ++ codigoItem) (Falha erro)
        in Left (erro, logNaoEncontrado)
    
    | quant > quantAtual =
        let
            erro = "Estoque insuficiente para remover essa quantidade!"
            logExcesso = LogEntry horario Remove ("Erro ao remover estoque do item: " ++ codigoItem) (Falha erro)
        in Left (erro, logExcesso)
    
    | otherwise =
        let
            novaQuant = quantAtual - quant
            novoInv = Map.adjust (\i -> i {quantidade = novaQuant}) codigoItem inventario
            logRemoveu = LogEntry horario Remove ("Item com quantidade removida: " ++ codigoItem) Sucesso
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
            logVazio = LogEntry horario QueryFail ("Erro: Inventario vazio") (Falha erro)
        in Left(erro, logVazio)
    | otherwise =
        let 
            stringsInventario = map (\item -> "ID: " ++ itemID item ++
                                     ", Nome: " ++ nome item ++
                                     ", Quantidade: " ++ show (quantidade item) ++
                                     ", Categoria: " ++ categoria item) (Map.elems inventario)
            logSucesso = LogEntry horario Query ("Inventario consultado com sucesso") Sucesso
        in Right(stringsInventario, logSucesso)
        
formatarLogs :: [LogEntry] -> [String]
formatarLogs listaLogs = map (\logentry -> "Horario: " ++ show (timestamp logentry) ++
                                        ", Acao: " ++ show (acao logentry) ++
                                        ", Detalhes: " ++ detalhes logentry ++
                                        ", Status: " ++ formatarStatus logentry) listaLogs
    where 
        formatarStatus :: LogEntry -> String
        formatarStatus logentry =
            case status logentry of
                Sucesso -> "Sucesso"
                Falha msg -> "Falha: " ++ msg


updateQty :: UTCTime -> Inventario -> String -> Int -> Either (String, LogEntry) ResultadoOperacao
updateQty horario inventario codigoItem novaQuant
    | not (Map.member codigoItem inventario) =
        let
            erro = "Item " ++ codigoItem ++ " nao encontrado"
            logNaoEncontrado = LogEntry horario Update ("Erro ao atualizar item: " ++ codigoItem) (Falha erro)
        in Left (erro, logNaoEncontrado)
    
    | novaQuant < 0 =
        let
            erro = "Quantidade " ++ show novaQuant ++ " invalida!"
            logQuantInvalida = LogEntry horario Update ("Erro ao atualizar item: " ++ codigoItem) (Falha erro)
        in Left(erro, logQuantInvalida)
    
    | otherwise =
        let
            novoInv = Map.adjust (\i -> i {quantidade = novaQuant}) codigoItem inventario 
            logUpdate = LogEntry horario Update ("Item com quantidade alterada: " ++ codigoItem) Sucesso
        in Right(novoInv, logUpdate)
        
        
deletaItem :: UTCTime -> Inventario -> String -> Either (String, LogEntry) ResultadoOperacao
deletaItem horario inventario codigoItem 
    | not (Map.member codigoItem inventario) =
        let 
            erro = "Item " ++ codigoItem ++ " nao encontrado"
            logNaoEncontrado =  LogEntry horario Delete ("Erro ao deletar item: " ++ codigoItem) (Falha erro)
        in Left(erro, logNaoEncontrado)
    
    | otherwise =
        let 
            novoInv = Map.delete codigoItem inventario
            logDeletou = LogEntry horario Delete ("Item deletado: " ++ codigoItem) Sucesso
        in Right(novoInv, logDeletou)


historicoPorItem :: [LogEntry] -> String -> Either String [LogEntry]
historicoPorItem listaLogs itemProcurado
    | null listaLogs =
        Left "Nenhum log foi encontrado"
    | otherwise =
        case iterarHistoricos listaLogs itemProcurado of
            [] -> Left "Nenhum log foi encontrado para esse item"
            listaHistorico -> Right listaHistorico
        
    where
        iterarHistoricos :: [LogEntry] -> String -> [LogEntry]
        iterarHistoricos [] _ = []
        iterarHistoricos (logentry:resto) itemProcurado =
            case status logentry of
                Falha _ -> 
                    let codigoItem = case acao logentry of
                                        Add -> drop 22 (detalhes logentry)
                                        Remove -> drop 33 (detalhes logentry)
                                        Update -> drop 24 (detalhes logentry)
                                        Delete -> drop 22 (detalhes logentry)
                                        _ -> ""
                    in
                        if codigoItem == itemProcurado
                            then logentry : iterarHistoricos resto itemProcurado
                            else iterarHistoricos resto itemProcurado
                Sucesso ->
                    let codigoItem = case acao logentry of
                                        Add -> drop 17 (detalhes logentry)
                                        Remove -> drop 30 (detalhes logentry)
                                        Update -> drop 30 (detalhes logentry)
                                        Delete -> drop 15 (detalhes logentry)
                                        _ -> ""
                    in
                        if codigoItem == itemProcurado
                            then logentry : iterarHistoricos resto itemProcurado
                            else iterarHistoricos resto itemProcurado

logsDeErro :: [LogEntry] -> Either String [LogEntry]
logsDeErro listaLogs
    | null listaLogs =
        Left "Nenhum log foi encontrado"
    | otherwise =
        case iterarErros listaLogs of
            [] -> Left "Nenhum log de erro foi encontrado"
            listaErros -> Right listaErros
        
    where
        iterarErros :: [LogEntry] -> [LogEntry]
        iterarErros [] = []
        iterarErros (logentry:resto) =
            case status logentry of
                Falha _ ->
                    logentry : iterarErros resto
                Sucesso ->
                    iterarErros resto
                                    

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
    hPutStr arquivo (unlines (logParaLinhas logSalvar))
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

lerQuant :: String -> IO Int 
lerQuant pergunta = do
    putStrLn pergunta
    hFlush stdout
    
    quantidadeString <- getLine
    case readMaybe quantidadeString :: Maybe Int of 
        Nothing -> do
            putStrLn "Entrada invalida - insira um numero inteiro!"
            quantidade <- lerQuant pergunta
            return quantidade
            
        Just quantidade -> do
            return quantidade 
    

-- #############################################################################

-- ### CAMINHOS DOS ARQUIVOS ###

arquivoInv :: FilePath
arquivoInv = "Inventario.dat"

arquivoLog :: FilePath
arquivoLog = "Auditoria.log"

-- #############################

-- ########################### FUNCOES DA  MAIN ################################



    
popularInventario :: IO Inventario
popularInventario = do 

    let inventario0 = Map.empty
    
    horario <- getCurrentTime
        
    let 
        item1 = Item "001" "Pão" 5 "Comida"
        item2 = Item "002" "Cenoura" 5 "Comida"
        item3 = Item "003" "Batata" 5 "Comida"
        item4 = Item "004" "Alho" 5 "Comida"
        item5 = Item "005" "Cebola" 5 "Comida"
        item6 = Item "006" "Terra" 5 "Material"
        item7 = Item "007" "Pedras" 5 "Material"
        item8 = Item "008" "Asfalto" 5 "Material"
        item9 = Item "009" "Madeira" 5 "Material"
        item10 = Item "010" "Teclado Razer 3001" 5 "Produto Comercial"
    
    inv1  <- adicionar horario inventario0 item1
    inv2  <- adicionar horario inv1 item2
    inv3  <- adicionar horario inv2 item3
    inv4  <- adicionar horario inv3 item4
    inv5  <- adicionar horario inv4 item5
    inv6  <- adicionar horario inv5 item6
    inv7  <- adicionar horario inv6 item7
    inv8  <- adicionar horario inv7 item8
    inv9  <- adicionar horario inv8 item9
    inv10 <- adicionar horario inv9 item10
    
    salvarInventario arquivoInv inv10
    
    return inv10

    where
        adicionar :: UTCTime -> Inventario -> Item -> IO Inventario
        adicionar horario inventario item =
        
            case addItem horario item inventario of
                Left (_, logErro) -> do
                    salvarLog logErro
                    return inventario
                    
                Right (novoInv, logSucesso) -> do
                    salvarLog logSucesso
                    return novoInv

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
            listaLogs <- carregarLogs arquivoLog
            acaoReport listaLogs
            loopPrincipal inventario
            
        "exit" -> do
            putStrLn "SALVANDO E FECHANDO SISTEMA"
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

    quantidadeItem <- lerQuant "Insira a quantidade inicial do Item a ser adicionado: "
    
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
    
    quantidadeItem <- lerQuant "Insira a quantidade a ser subtraida: "
    
    horario <- getCurrentTime
    case removeItem horario codigoItem quantidadeItem inventario of
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

    quantidadeItem <- lerQuant "Insira a quantidade a ser atribuida: "
    
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
            return inventario
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
            
acaoReport :: [LogEntry] -> IO()
acaoReport listaLogs = do
    putStrLn "Digite o id do item a ser procurado: "
    hFlush stdout
    codigoItem <- getLine
    putStrLn ("\n## Historico Do Item " ++ codigoItem ++ " ##")
    case historicoPorItem listaLogs codigoItem of
        Left erro -> print erro
        Right listaHistorico -> imprimirLogs (formatarLogs listaHistorico)
        
    putStrLn ("\n## Logs de erro ##")
    case logsDeErro listaLogs of
        Left erro -> print erro
        Right listaErros -> imprimirLogs (formatarLogs listaErros)
        
    putStrLn ("\n## Item mais movimentado ##")
    case itemMaisMovimentado listaLogs of
        Left erro -> print erro
        Right item -> -------
    
    where 
        imprimirLogs :: [String] -> IO()
        imprimirLogs [] = return ()
        imprimirLogs (logentry:resto) = do
            print logentry
            imprimirLogs resto
    
    
itemMaisMovimentado :: [LogEntry] -> String
itemMaisMovimentado listaLogs
    | null listaLogs =
        Left "Nenhum log foi encontrado"
    | otherwise =
        case 
        
    


main :: IO ()
main = do
    putStrLn "=== INICIO DO SISTEMA ==="

    existeInv <- doesFileExist arquivoInv
    existeLog <- doesFileExist arquivoLog

    inventario <- if existeInv
        then carregarInventario arquivoInv
        else popularInventario

    loopPrincipal inventario
            
-- #############################################################################