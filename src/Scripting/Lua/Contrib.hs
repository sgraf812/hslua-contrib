{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Scripting.Lua.Contrib where

import Scripting.Lua
       (LuaState, StackValue, LuaNumber, LuaInteger, LuaCFunction)

import qualified Scripting.Lua as Lua
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Tagged
import Data.Proxy
import Data.Functor.Identity
import Data.ByteString

class (MonadReader LuaState m, MonadIO m, MonadMask m) =>
      MonadInterpreter m

newtype InterpreterT m a = InterpreterT
  { unInterpreterT :: ReaderT LuaState m a
  } deriving (Functor, Applicative, Monad, MonadReader LuaState, MonadIO, MonadThrow, MonadCatch, MonadMask)

instance (MonadIO m, MonadMask m) =>
         MonadInterpreter (InterpreterT m)

type Interpreter = InterpreterT IO

runInterpreterT
  :: (MonadIO m, MonadMask m)
  => LuaState -> InterpreterT m a -> m a
runInterpreterT l = flip runReaderT l . unInterpreterT

runInterpreter :: LuaState -> Interpreter a -> IO a
runInterpreter = runInterpreterT

interpretFreshT
  :: (MonadIO m, MonadMask m)
  => InterpreterT m a -> m a
interpretFreshT i = do
  l <- liftIO Lua.newstate
  runInterpreterT l i

interpretFresh :: Interpreter a -> IO a
interpretFresh = interpretFreshT

newtype LuaFunc sig = LuaFunc
  { unLuaFunc :: String
  }

declareFunc :: String -> LuaFunc sig
declareFunc = LuaFunc

funcName :: LuaFunc sig -> String
funcName = unLuaFunc

reinferSig :: LuaFunc sig1 -> LuaFunc sig2
reinferSig = declareFunc . funcName

lift0
  :: MonadInterpreter m
  => (LuaState -> IO r) -> m r
lift0 f = ask >>= \s -> liftIO (f s)

lift1
  :: MonadInterpreter m
  => (LuaState -> a -> IO r) -> a -> m r
lift1 f a = ask >>= \s -> liftIO (f s a)

lift2
  :: MonadInterpreter m
  => (LuaState -> a -> b -> IO r) -> a -> b -> m r
lift2 f a b = ask >>= \s -> liftIO (f s a b)

lift3
  :: MonadInterpreter m
  => (LuaState -> a -> b -> c -> IO r) -> a -> b -> c -> m r
lift3 f a b c = ask >>= \s -> liftIO (f s a b c)

openlibs
  :: MonadInterpreter m
  => m ()
openlibs = lift0 Lua.openlibs

getglobal
  :: MonadInterpreter m
  => String -> m ()
getglobal = lift1 Lua.getglobal

push
  :: (MonadInterpreter m, StackValue a)
  => a -> m ()
push = lift1 Lua.push

peek
  :: (MonadInterpreter m, StackValue a)
  => Int -> m (Maybe a)
peek = lift1 Lua.peek

gettop
  :: MonadInterpreter m
  => m Int
gettop = lift0 Lua.gettop

pop :: MonadInterpreter m => Int -> m ()
pop = lift1 Lua.pop

pcall :: MonadInterpreter m => Int -> Int -> Int -> m Int
pcall = lift3 Lua.pcall

type family Interpreted m sig where
        Interpreted m (a -> r) = a -> Interpreted m r
        Interpreted m a = m a

call
  :: forall m sig.
     (MonadInterpreter m, CallableSig sig)
  => LuaFunc sig -> Interpreted m sig
call = call' (return () :: m ()) 0

class CallableSig sig  where
  call'
    :: MonadInterpreter m
    => m () -> Int -> LuaFunc sig -> Interpreted m sig
  default call' :: (MonadInterpreter m, StackValue sig) => m () -> Int -> LuaFunc sig -> m sig
  call' pushArgs n f = do
    getglobal (funcName f)
    pushArgs
    lift2 Lua.call n 1
    i <- gettop
    Just val <- peek i
    pop 1
    return val

instance (StackValue a, CallableSig r) =>
         CallableSig (a -> r) where
  call' pushArgs n f a =
    call' (pushArgs >> push a) (n + 1) (reinferSig f :: LuaFunc r)

instance CallableSig () where
  call' pushArgs n f = do
    getglobal (funcName f)
    pushArgs
    pcall n 0 0
    return ()

instance (StackValue a, StackValue b, CallableSig a, CallableSig b) => CallableSig (a, b) where
  call' pushArgs n f = do
    getglobal (funcName f)
    pushArgs
    pcall n 2 0
    i <- gettop
    Just a <- peek i
    Just b <- peek (i - 1)
    pop 2
    return (a, b)

instance CallableSig Bool

instance CallableSig Int

instance CallableSig ByteString

instance CallableSig LuaNumber

instance CallableSig LuaInteger

instance CallableSig LuaState

instance (StackValue a, CallableSig a) =>
         CallableSig [a]
