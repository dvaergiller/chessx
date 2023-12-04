module Main where

import Network.Wai.Handler.Warp

import ChessX.Server

main :: IO ()
main = application >>= run 8081
