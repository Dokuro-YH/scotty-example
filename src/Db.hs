{-# LANGUAGE OverloadedStrings #-}

module Db where

import           Types

import           Control.Monad

import           Database.MySQL.Simple (Connection, Only (..), execute,
                                        execute_, insertID, query, query_)

todoMapper :: (Int, String, String) -> Todo
todoMapper (id, name, status) = Todo id name status

selectTodoList :: Connection -> IO [Todo]
selectTodoList conn = liftM (map todoMapper) $ query_ conn "select * from todo"

selectTodo :: Int -> Connection -> IO (Maybe Todo)
selectTodo id conn = do
  tm <- query conn "select * from todo where id=?" [id]
  return $ case tm of
    [x] -> Just $ todoMapper x
    _   -> Nothing

insertTodo :: Todo -> Connection -> IO Todo
insertTodo (Todo _ name status) conn = do
  execute conn "insert into todo(name,status) value(?,?)" [name, status]
  id <- insertID conn
  return $ Todo (fromIntegral id) name status

updateTodo :: Todo -> Int -> Connection -> IO (Maybe Todo)
updateTodo (Todo _ name status) id conn = do
  r <- execute conn "update todo set name=?, status=? where id=?" [name,status,show id]
  return $ if r > 0
           then Just $ Todo id name status
           else Nothing

deleteTodo :: Int -> Connection -> IO ()
deleteTodo id conn = do
  execute conn "delete from todo where id=?" [id]
  return ()
