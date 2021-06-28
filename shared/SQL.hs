{-# LANGUAGE RecordWildCards #-}

module SQL where

import Data.List (intercalate)

-- Tiny String Builder
newtype SqlCol = SqlCol {sqlCol :: String} deriving Show
newtype SqlCond = SqlCond {sqlCond :: String} deriving Show
newtype SqlFrom = SqlFrom {sqlFromTable :: String} deriving Show -- TODO: expand this representation
data SqlOrder = SqlAscending SqlCol | SqlDescending SqlCol | SqlDirFunction String deriving Show

data SqlQuery = SqlQuery
  { sqlSelect :: [SqlCol],
    sqlFrom :: SqlFrom,
    sqlWhere :: [SqlCond],
    sqlOrder :: Maybe SqlOrder,
    sqlLimit :: Maybe Int
  } deriving (Show)

defaultQuery = SqlQuery {
  sqlSelect = [],
  sqlFrom = SqlFrom "",
  sqlWhere = [],
  sqlOrder = Nothing,
  sqlLimit = Nothing
}

sql2string :: SqlQuery -> String
sql2string SqlQuery {..} =
  "SELECT"
    ++ (intercalate "," (sqlCol <$> sqlSelect))
    ++ (sqlFromTable sqlFrom)
    ++ " "
    ++ whereClause
    ++ " "
    ++ orderClause
    ++ " "
    ++ limitClause
  where
    whereClause = case sqlWhere of
      [] -> ""
      lst -> " " ++ (intercalate " AND " (sqlCond <$> lst)) ++ " "
    orderClause = case sqlOrder of
      Nothing -> ""
      Just (SqlAscending (SqlCol colName)) -> "ORDER BY " ++ colName
      Just (SqlDescending (SqlCol colName)) -> "ORDER BY " ++ colName ++ "DESC"
    limitClause = case sqlLimit of
      Nothing -> ""
      Just n -> "LIMIT " ++ show n

bracketQuery :: FromRow r => String -> IO [r]
bracketQuery queryString = do
  conn <- open dbFile
  r <- query_ conn (Query . pack $ queryString)
  close conn
  pure r

-- | Wrapper for sql query execution
bracketExecute :: String -> IO ()
bracketExecute queryString = do
  conn <- open dbFile
  execute_ conn (Query . pack $ queryString)
  close conn

-- | Given a list of table names, drop them
dropTables tables = mapM_ (\table -> bracketExecute $ "DROP TABLE IF EXISTS " ++ table ++ ";") tables 

data Index = Index {
  indexName :: String,
  indexTable :: String,
  indexField :: String,
  indexUnique :: Bool
} deriving Show

-- | Create an index
createIndex Index{..} = 
  if indexUnique then
    bracketExecute $ "CREATE INDEX " ++ indexName ++ " on " ++ indexTable ++ "(" ++ indexField ++ ");"
  else
    bracketExecute $ "CREATE UNIQUE INDEX " ++ indexName ++ " on " ++ indexTable ++ "(" ++ indexField ++ ");"

-- | Create indices
createIndices :: [Index] -> IO ()
createIndices indexList = mapM_ createIndex indexList
