{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Demo.Common

import System.Environment ( getArgs )

{----------------------------------------------------------------------
-- USB operations.
----------------------------------------------------------------------}

-- | Parse command line arguments.
--
-- * if @saveraw@ is given, run 'writeRawDataToDisk';
--
-- * if @inspect@ is given, run 'inspectData';
--
-- * otherwise, run 'testVideoStream';
--
main ∷ IO ()
main = do
    let usage = "Usage: test [options]\n\
                \\t-h|--help) display this help\n\
                \\tsaveraw)   get default video stream and save to disk\n\
                \\tinspect)   get default video stream and display headers\n\
                \\t[default]  prompt for configuration and save to disk\n"

    args ← getArgs
    let action = case args of
         ("-h":_)      → putStr usage
         ("--help":_)  → putStr usage
         ("saveraw":_) → writeRawDataToDisk
         ("inspect":_) → inspectData
         _             → testVideoStream

    catchCommonUSBException action
