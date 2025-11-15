import qualified Data.Map as Map
import Data.Time (UTCTime, getCurrentTime)

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

criarLogEntry :: UTCTime -> AcaoLog -> String -> StatusLog -> LogEntry
criarLogEntry hora acao detalhes status =
    LogEntry{
        timestamp = hora,
        acao = acao,
        detalhes = detalhes,
        status = status
    }
    

-- ####################### FUNÇÕES PURAS DE NEGOCIO ############################

addItem :: UTCTime -> Item -> Inventario -> Either (String, LogEntry) ResultadoOperacao
addItem horario item inventario
    | Map.member (itemID item) inventario =
        let
            erro = "O item " ++ itemID item ++ " ja esta registrado no inventario"
            logErro = criarLogEntry horario Add ("Falha: " ++ erro) (Falha erro)
        in Left (erro, logErro)
    
    | otherwise =
        let
            novoInv = Map.insert (itemID item) item inventario
            logSucesso = criarLogEntry horario Add ("Item adicionado: " ++ nome item) Sucesso
        in Right (novoInv, logSucesso)
        

removeItem :: UTCTime -> String -> Int -> Inventario -> Either (String, LogEntry) ResultadoOperacao
removeItem horario codigoItem quant inventario 
    | not (Map.member codigoItem inventario) =
        let
            erro = "Item " ++ codigoItem ++ " nao encontrado"
            logNaoEncontrado = criarLogEntry horario Remove ("Falha: " ++ erro) (Falha erro)
        in Left (erro, logNaoEncontrado)
    
    | quant > quantAtual =
        let
            erro = "Estoque insuficiente para remover essa quantidade!"
            logExcesso = criarLogEntry horario Remove ("Falha: " ++ erro) (Falha erro)
        in Left (erro, logExcesso)
    
    | otherwise =
        let
            novaQuant = quantAtual - quant
            novoInv = Map.adjust (\i -> i {quantidade = novaQuant}) codigoItem inventario
            logRemoveu = criarLogEntry horario Remove ("Removido " ++ show quant ++ " unidades do item " ++ codigoItem ++ 
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
            logVazio = criarLogEntry horario QueryFail ("Falha: " ++ erro) (Falha erro)
        in Left(erro, logVazio)
    | otherwise =
        let 
            stringsInventario = map (\item -> "ID: " ++ itemID item ++
                                     ", Nome: " ++ nome item ++
                                     ", Quantidade: " ++ show (quantidade item) ++
                                     ", Categoria: " ++ categoria item) (Map.elems inventario)
            logSucesso = criarLogEntry horario Query ("Inventario consultado com sucesso") Sucesso
        in Right(stringsInventario, logSucesso)


updateQty :: UTCTime -> Inventario -> String -> Int -> Either (String, LogEntry) ResultadoOperacao
updateQty horario inventario codigoItem novaQuant
    | not (Map.member codigoItem inventario) =
        let
            erro = "Item " ++ codigoItem ++ " nao encontrado"
            logNaoEncontrado = criarLogEntry horario Update ("Falha: " ++ erro) (Falha erro)
        in Left (erro, logNaoEncontrado)
    
    | novaQuant < 0 =
        let
            erro = "Quantidade " ++ show novaQuant ++ " invalida!"
            logQuantInvalida = criarLogEntry horario Update ("Falha: " ++ erro) (Falha erro)
        in Left(erro, logQuantInvalida)
    
    | otherwise =
        let
            novoInv = Map.adjust (\i -> i {quantidade = novaQuant}) codigoItem inventario 
            logUpdate = criarLogEntry horario Update ("Quantidade do item " ++ codigoItem ++ " alterada para: " ++ show novaQuant) Sucesso
        in Right(novoInv, logUpdate)
        
        
deletaItem :: UTCTime -> Inventario -> String -> Either (String, LogEntry) ResultadoOperacao
deletaItem horario inventario codigoItem 
    | not (Map.member codigoItem inventario) =
        let 
            erro = "Item " ++ codigoItem ++ " nao encontrado"
            logNaoEncontrado =  criarLogEntry horario Delete ("Falha: " ++ erro) (Falha erro)
        in Left(erro, logNaoEncontrado)
    
    | otherwise =
        let 
            novoInv = Map.delete codigoItem inventario
            logDeletou = criarLogEntry horario Delete ("Item " ++ codigoItem ++ " foi deletado!") Sucesso
        in Right(novoInv, logDeletou)

-- #############################################################################

-- ######## FUNÇÕES DE I/O - ENVOLVEM EFEITOS COLATERAIS #######################

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

itemParaLinhas :: Item -> [String]
itemParaLinhas item =
    [ itemID item
    , nome item
    , show (quantidade item)
    , categoria item
    ]


-- #############################################################################

-- ################################# MAIN ######################################
main :: IO()
main = do 
    horarioAgr <- getCurrentTime
    
    let itemTesteUm = Item "001" "Soja" 100 "Grao"
    let itemTesteDois = Item "002" "Palmito" 12 "Seila"
    let itemTesteTres = Item "003" "Cafe" 10 "Grao"
    
    let inventarioTeste = Map.empty
    
    case addItem horarioAgr itemTesteUm inventarioTeste of
        Left (erro, _) -> print erro
        Right (novoInv, _) -> do
            print "uhuu"
                    
            case addItem horarioAgr itemTesteDois novoInv of
                Left (erro, _) -> print erro
                Right (novoInv2, _) -> do
                    print "uhuu 2"
                    
                    case addItem horarioAgr itemTesteTres novoInv2 of
                        Left (erro, _) -> print erro
                        Right (novoInv3, _) -> do
                            print "uhuu yeahh uh 3"
                                    
                            case consultarInventario horarioAgr novoInv3 of
                                Left (erro, _) -> print erro
                                Right (consulta, _) -> do
                                    print consulta
                                            
                                            
                                                    
                                                                            
                                                                    

