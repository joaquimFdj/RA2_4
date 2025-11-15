import qualified Data.Map as Map
import Data.Time (UTCTime)

data Item = Item {
    itemID :: String,
    nome :: String,
    quantidade :: Int,
    categoria :: String
} deriving (Show, Read)

type Inventario = Map.Map String Item

data AcaoLog = Add | Remove | Update | QueryFail
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
    

addItem :: UTCTime -> Item -> Inventario -> Either (String, LogEntry) ResultadoOperacao
addItem horario item inventario
    | Map.member (itemID item) inventario =
        let erro = "O item " ++ itemID item ++ " ja esta registrado no inventario"
            logErro = criarLogEntry horario Add erro (Falha erro)
        in Left (erro, logErro)
    
    | otherwise =
        let novoInv = Map.insert (itemID item) item inventario
            logSucesso = criarLogEntry horario Add ("Item adicionado: " ++ nome item) 
        in Right (novoInv, logSucesso)
        

removeItem :: UTCTime -> String -> Int -> Inventario -> Either (String, LogEntry) ResultadoOperacao
removeItem horario codigoItem quant inventario 
    | not (Map.member codigoItem inventario) =
        let erro = "Item " ++ codigoItem ++ " nao encontrado"
            logNaoEncontrado = criarLogEntry horario Remove ("Falha: " ++ erro) (Falha erro)
        in Left (erro, logNaoEncontrado)
    
    | quant > quantAtual =
        let erro = "Estoque insuficiente!"
            logExcesso = criarLogEntry horario Remove ("Falha: " ++ erro) (Falha erro)
        in Left (erro, logExcesso)

    | quant == quantAtual =
        let novoInv = Map.delete codigoItem inventario
            logRemoveu = criarLogEntry horario Remove ("Item " ++ codigoItem ++ " removido do inventario!") Sucesso
        in Right(novoInv, logRemoveu)
    
    | otherwise =
        let novaQuant = quantAtual - quant
            novoInv = Map.adjust (\i -> i {quantidade = novaQuant}) codigoItem inventario
            logRemoveu = criarLogEntry horario Remove ("Removido " ++ show quant ++ " unidades do item " ++ codigoItem ++ 
                        ". Estoque anterior: " ++ show quantAtual ++ 
                        ", estoque atual: " ++ show novaQuant) Sucesso
        in Right(novoInv, logRemoveu)
            
    where 
        quantAtual = case Map.lookup codigoItem inventario of
            Just item -> quantidade item
            Nothing -> 0





