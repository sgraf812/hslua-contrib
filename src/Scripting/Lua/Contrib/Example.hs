{-# LANGUAGE OverloadedStrings #-}

module Scripting.Lua.Contrib.Example where

import Data.ByteString
import Scripting.Lua.Contrib (LuaFunc)
import qualified Scripting.Lua.Contrib as Lua

lua'print :: LuaFunc (ByteString -> ())
lua'print = Lua.declareFunc "print"

sayHi :: IO ()
sayHi = Lua.interpretFresh $ do
  Lua.openlibs
  Lua.call lua'print "hi"
