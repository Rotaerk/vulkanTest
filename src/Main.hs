{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Prelude hiding (init)
import Control.Exception
import Control.Monad
import Control.Monad.Loops
import Data.Function
import Graphics.UI.GLFW (Window, WindowHint(..), ClientAPI(..))
import qualified Graphics.UI.GLFW as GLFW

main :: IO ()
main =
  (
    initWindow >>= \case
      Just window -> do
        initVulkan
        mainLoop window
        cleanup window
      Nothing ->
        putStrLn "Failed to initialize the GLFW window."
  )
  `catch` (
    \(e :: IOException) ->
      putStrLn $ displayException e
  )

(width, height) = (800, 600);

initWindow :: IO (Maybe Window)
initWindow = do
  GLFW.init
  GLFW.windowHint $ WindowHint'ClientAPI ClientAPI'NoAPI
  GLFW.windowHint $ WindowHint'Resizable False
  GLFW.createWindow width height "Vulkan" Nothing Nothing

initVulkan :: IO ()
initVulkan = return ()

mainLoop :: Window -> IO ()
mainLoop window = whileM_ (not <$> GLFW.windowShouldClose window) GLFW.pollEvents

cleanup :: Window -> IO ()
cleanup window = do
  GLFW.destroyWindow window
  GLFW.terminate
