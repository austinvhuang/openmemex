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
