# **Sistema de Inventario em Haskell**

## -Pontifícia Universidade Católica do Paraná-

## -Programação Lógica e Funcional-

## **Professor:**

### Frank de Alcantara

## **Alunos:**

### - Artur Moretti Zimmermann (artur-mizi)

### - Bruno Navarro Ivatiuk (BrunoIvatiuk)

### - Joaquim dos Anjos Faraco (joaquimFdj)

# TESTES MANUAIS REQUISITADOS NAS ESPECIFICAÇÕES

## Cenário 1: Persistência de Estado:
- INICIAR O PROGRAMA

- ADICIONAR 3 ITENS

<img width="521" height="136" alt="cenario1_1" src="https://github.com/user-attachments/assets/5e3c5a57-9daf-46e2-ac22-2f062d6961d8" />

<img width="516" height="131" alt="cenario1_2" src="https://github.com/user-attachments/assets/0c378d25-153a-4333-acd4-dbfcd8d4a75f" />

<img width="526" height="135" alt="cenario1_3" src="https://github.com/user-attachments/assets/4ee40411-4bca-41f2-8326-a276ab3df632" />

- VERIFICAR SE ARQUIVOS FORAM CRIADOS

<img width="307" height="24" alt="cenario1_4" src="https://github.com/user-attachments/assets/6d89054f-bb2c-4d6a-8762-70df3100cc1f" />

- REINICIAR E VERIFICAR MEMORIA DO DISCO

DETALHE: O print mostra 13 itens, pois ao rodar o código sem arquivo, ele automaticamente popula um inventário para o usuário

<img width="747" height="251" alt="cenario1_5" src="https://github.com/user-attachments/assets/32cd4e44-d370-403a-934f-1178ba47f83a" />

## Cenário 2: Erro de Lógica:
- ADICIONAR ARQUIVO COM 10 UNIDADES
<img width="539" height="128" alt="cenario2_1" src="https://github.com/user-attachments/assets/65ea2963-ee7e-459b-971c-96248a3cdd26" />

- TENTAR REMOVER 15 UNIDADES
- TERMINAL EXIBE ERRO CLARO
<img width="551" height="88" alt="cenario2_2" src="https://github.com/user-attachments/assets/b658a715-4739-4316-a308-d3e5d629f419" />

- VERIFICA SE Inventario.dat AINDA MOSTRA 10 UNIDADES
<img width="146" height="75" alt="cenario2_3" src="https://github.com/user-attachments/assets/6f644e53-c2e2-4d9e-877d-8eb64d3b9e89" />

- VERIFICA LOG DE ERRO EM Auditoria.log
<img width="564" height="78" alt="cenario2_4" src="https://github.com/user-attachments/assets/cb53f2f4-2faa-42ea-ae95-3166aabb15d1" />

## Cenário 3: Geração de Relatório de Erros:
- APOS CENARIO 2, EXECUTAR REPORT
- VERIFICAR SE CONSTA ERRO DO CENARIO 2
<img width="1484" height="189" alt="cenario3" src="https://github.com/user-attachments/assets/b5dac106-1597-47b8-86ff-dc978871c607" />


# COMO RODAR O CÓDIGO
Apenas acessar o seguinte link do onlineGDB e executar o código:

https://onlinegdb.com/82TFsu8Om

# Para que serve?

O programa em Haskell, hospedado no onlineGDB, tem como finalidade simular um sistema de inventário, com itens, categorias, estoque, funções de CRUD para manipular o inventário como um todo, sistema de logs para registrar mudanças feitas durante a execução, e leitura e escrita de arquivos, garantindo a permanência de informações mesmo após o encerramento da execução.

# Como funciona?

O código conta com diversas funções, tanto puras quanto de I/O, que trabalham em conjunto para realizar as ações necessárias, e as comunicar ao usuário com clareza e transparência. Alguns exemplos são:

# Tipos:

Antes de começar a listar as funções do código, é importante apontar os tipos antes, para saber com exatidão as variáveis e valores que são manipuladas.

### Item
Os itens são a base de todo o sistema, cada um possui seu próprio ID (tratado para ser exclusivo), nome, quantidade em estoque, e uma categoria. Todos esses valores compõe um item.
```
data Item = Item {
    itemID :: String,
    nome :: String,
    quantidade :: Int,
    categoria :: String
} deriving (Show, Read)
```
### Inventario
O inventario é onde todos os itens são guardados, sendo um Map onde cada entrada representa um dos Itens existentes no sistema.
```
type Inventario = Map.Map String Item
```
### LogEntry
Os LogEntry<sub>s</sub> são gerados toda vez que alguma alteração é feita no sistema, ou também com falhas, elas carregam em si o horário real do Log, a ação que gerou o log, os detalhes (geralmente a qual item a ação foi aplicada), e o status (sendo Sucesso ou Falha, que vem acompanhada da mensagem de erro).
```
data LogEntry = LogEntry {
    timestamp :: UTCTime,
    acao :: AcaoLog,
    detalhes :: String,
    status :: StatusLog
} deriving (Show, Read)
```
### AcaoLog
Esse é o "atributo" da LogEntry que carrega a ação que gerou aquele Log especifico, esse tipo deriva Eq para que possamos comparar e definir qual a ação de cada Log em funções específicas.
```
data AcaoLog = Add | Remove | Update | Query | QueryFail
    deriving (Show, Read, Eq)
```
### StatusLog
Já o statuslog, mostra se a operação do Log ocorreu como o esperado, ou se houve algum erro na execução do comando. Quando é uma falha, a mesma vem acompanhada de uma string com os detalhes do erro, que variam com a operação e a entrada do usuário.
```
data StatusLog = Sucesso | Falha String
    deriving (Show, Read, Eq)
```
### ResultadoOperacao
Esse tipo serve apenas para simplificar o código, facilitando o retorno das funções que alteram o inventario e tem que constantemente retornar um Inventario e um LogEntry.
```        
type ResultadoOperacao = (Inventario, LogEntry)
```

# Funções Puras:
Uma função é determinada pura quando ela só interage com a lógica interna do sistema e dos tipos, sem o uso de entradas do usuário e sem retornar informação alguma na tela, apenas internamente.

## Funções CRUD
Essas são as funções que estão encarregadas de realmente alterar o estado o inventário e dos itens que lá residem, são as funções principais do sistema, que serão chamadas praticamente toda execução.


### addItem
É uma adição básica, recebe todos os atributos que um item precisa ter e o agrega ao inventário que foi passado junto, trata o erro caso o ID desejado já esteja inserido no inventário, caso contrário, o adiciona.
```
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
```        

### removeItem
Essa função recebe o ID de um item existente e uma quantidade em inteiro, a qual será subtraída do estoque atual do item, gerando assim uma nova quantidade, essa função trata o erro do ID do item não estar registrado no inventário, e também caso o valor a ser subtraído seja maior que o atual registrado como estoque do item.
```
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
```

### updateQty
Embora essa função se pareça com a anterior, elas têm propósitos diferentes. Enquanto removeItem subtrai uma quantia do estoque registrado, updateQty recebe o inteiro do usuário e o atribui diretamente ao estoque do item, podendo assim designar mais facilmente o estoque desejado. Essa função trata o item não existir e números negativos.
```
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
```

## Funções para Impressão
Funções que não retornam diretamente no terminal, mas são usadas para formatar o Inventário e a lista de LogEntry<sub>s</sub> para ficarem legíveis, com atributos destacados e afins.

### consultarInventario:
Recebe o inventário e, para cada item, separa seus atributos, também usa show em quantidade, para passar o inteiro em string, tornando imprimível. Gera um Log de Query ou QueryFail, caso o inventario esteja vazio.
```
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
```
### formatarLogs:
A partir da lista de LogEntry<sub>s</sub>, separa cada entrada do log em seus atributos e devolve como lista de Strings, para a impressão.
```
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
```
## Funções do report
Essas funções são usadas exclusivamente na ação report do loop principal, o propósito delas é analisar a lista de Logs e devolver filtragens e análises com base nos mesmos, as três são chamadas juntas e criam um relatório completo.

### historicoPorItem:
Recebe a lista de Logs e o ID de um item e retorna todas as instâncias de Logs que foram geradas com esse item, isso é possível graças à padronização dos detalhes dos logs para cada ação, que contém um número fixo de caracteres antes do ID, então são usados drops para ignorar exatamente o número de caracteres antes do ID para descobrir a qual item se refere uma determinada entrada no Log, é usada uma recursão para percorrer por todas as entradas na lista de Logs.
```
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
                                        _ -> ""
                    in
                        if codigoItem == itemProcurado
                            then logentry : iterarHistoricos resto itemProcurado
                            else iterarHistoricos resto itemProcurado
```
### logsDeErro:
É a função mais simples do report, embora carregue um grande valor prático. Ela filtra todas as entradas na lista de Logs que constam erros, e as retorna, servindo de guia para o usuário saber onde e no quê ele errou.
```

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
```
### itemMaisMovimentado:
Essa função utiliza o mesmo esquema de extrair os IDs dos itens de cada entrada no log, a diferença é que, ao invés de compará-los com a entrada do usuário, ela recolhe todos os IDs de todos os logs que os possuem em uma lista de strings, e depois conta a frequencia de cada ID nessa lista, retornando o ID com mais aparições nos Logs.
```
itemMaisMovimentado :: [LogEntry] -> Either String String
itemMaisMovimentado listaLogs
    | null listaLogs =
        Left "Nenhum log foi encontrado"
    | otherwise =
        case coletarCodigos listaLogs of
            [] -> Left "Nenhum item foi movimentado"
            listaCodigos -> let listaFrequencias = contarFrequencias listaCodigos Map.empty
                                itemMaisFrequente = encontrarItem listaFrequencias
                            in Right itemMaisFrequente
    
    where
        coletarCodigos :: [LogEntry] -> [String]
        coletarCodigos [] = []
        coletarCodigos (logentry:resto) =
            case status logentry of
                Falha _ -> coletarCodigos resto
                Sucesso ->
                    let codigoItem = case acao logentry of
                                        Add -> drop 17 (detalhes logentry)
                                        Remove -> drop 30 (detalhes logentry)
                                        Update -> drop 30 (detalhes logentry)
                                        _ -> ""
                    in
                        if codigoItem /= ""
                            then codigoItem : coletarCodigos resto
                            else coletarCodigos resto
    
        contarFrequencias :: [String] -> Map.Map String Int -> Map.Map String Int
        contarFrequencias [] mapaFrequencias = mapaFrequencias
        contarFrequencias (codigo:resto) mapaFrequencias =
            let
                quantidadeCodigo = Map.findWithDefault 0 codigo mapaFrequencias
                novoMapaFrequencias = Map.insert codigo (quantidadeCodigo + 1) mapaFrequencias
            in
                contarFrequencias resto novoMapaFrequencias
    
        encontrarItem :: Map.Map String Int -> String
        encontrarItem mapa =
            acharMaior (Map.toList mapa) ("", -1)
            where
                acharMaior :: [(String, Int)] -> (String, Int) -> String
                acharMaior [] (codigoItem, _) = codigoItem
                acharMaior ((codigoItem, freqItem):resto) (codigoMaisFreq, maiorFreq)
                    | freqItem > maiorFreq = acharMaior resto (codigoItem, freqItem)
                    | otherwise = acharMaior resto (codigoMaisFreq, maiorFreq)
```
# Funções de IO
Funções de IO servem para ler e escrever em arquivos, tal qual exibir mensagens no terminal, no caso desse projeto, servem principalmente para garantir a permanência dos dados de cada execução, usando os arquivos "Inventario.dat" e "Auditoria.log".

## Inventario IO:
As funções IO do inventario interagem com o arquivo "Inventario.log", e o mantêm atualizado com o estado atual do disco, e também o oposto! carregarInventario recebe o arquivo e o converte em um inventario separando as linhas do Inventario.log e as convertendo em itens separados por um Map.

Já salvarInventario faz o contrário, recebe o arquivo e o inventario que estiver carregado na memora naquele instante e escreve os conteúdos do inventário no arquivo, isso é feito no fim de cada operação com sucesso.
```
carregarInventario :: FilePath -> IO Inventario
carregarInventario caminho = do
    conteudo <- readFile caminho
    let linhas = lines conteudo 
        itens = lerItens linhas
        inventario = Map.fromList [(itemID i, i) | i <- itens]
    return inventario
    
    where
        lerItens :: [String] -> [Item]
        lerItens [] = []
        lerItens (idItem:nome:quantidade:categoria:resto) =
            let item = Item idItem nome (read quantidade) categoria
            in item : lerItens resto
        lerItens _ = []

salvarInventario :: FilePath -> Inventario -> IO ()
salvarInventario caminho inventario =
    withFile caminho WriteMode $ \arquivo -> do
        let itens = Map.elems inventario
            linhas = concatMap itemParaLinhas itens
        hPutStr arquivo (unlines linhas)
        hFlush arquivo

    where
        itemParaLinhas :: Item -> [String]
        itemParaLinhas item =
            [ itemID item
            , nome item
            , show (quantidade item)
            , categoria item
            ]
```

## logEntry IO
Essas funções têm o mesmo propósito das do inventário, mas para os Logs, carregarLogs recebe o arquivo e o transforma em uma lista de LogEntry<sub>s</sub>, e salvarLog recebe apenas uma logEntry e usa um append no arquivo, o motivo dessa distinção é que um item no inventario pode ser deletado, então deve ter a possibilidade de ser reescrito, enquanto um Log nunca é deletado, logo, pode ser apenas adicionado ao final do arquivo.
```
carregarLogs :: FilePath -> IO [LogEntry]
carregarLogs caminho = do
    conteudo <- readFile caminho
    length conteudo `seq` return ()
    let linhas = filter (not . null) (lines conteudo)
        logs = lerLogs linhas 
    return logs
    
    where
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
salvarLog logSalvar =
    withFile "Auditoria.log" AppendMode $ \arquivo -> do
        hPutStr arquivo (unlines (logParaLinhas logSalvar))

    where
        logParaLinhas :: LogEntry -> [String]
        logParaLinhas logSalvar =
            [ show (timestamp logSalvar)
            , show (acao logSalvar)
            , detalhes logSalvar
            , case status logSalvar of 
                Sucesso -> "Sucesso"
                Falha msg -> "Falha: " ++ msg
            ]
```
# MAIN
A fim de ser breve, e não repetir a mesma explicação / código várias vezes, a main será explicada sem exemplos de códigos, apenas com parágrafos descrevendo seu loop e funcionamento.

## Inicialização sem arquivos
Caso seja a primeira vez do usuário executando o arquivo, deve-se assumir que o mesmo não traz consigo arquivos Inventario.dat e Auditoria.log, para isso, foi criada uma função que automaticamente, ao perceber a ausência dos arquivos, gera um inventário com 10 itens base, seu arquivo, e o arquivo dos Logs com essas 10 adições já inclusas nas entradas, para que o usuário não precise popular manualmente o sistema antes de efetuar testes.

## Loop Principal
O loop principal segue um padrão comum em sistemas que operam com entradas no terminal, o programa exibe as opções ao usuário, que, ao escolher uma, é requisitado as entradas necessárias para as mesmas, isso é feito comparando a string recebida do usuário com as das opções, que chamam funções seguindo o padrão: acaoUm, acaoDois, [...] acaoReport. São essas funções "auxiliares" que mandam os inputs do usuario para as funções CRUD, e retornam uma mensagem no terminal informando o sucesso ou falha da operação.
