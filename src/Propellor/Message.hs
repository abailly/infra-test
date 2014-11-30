{-# LANGUAGE PackageImports #-}

module Propellor.Message where

import System.Console.ANSI
import System.IO
import System.Log.Logger
import System.Log.Formatter
import System.Log.Handler (setFormatter, LogHandler)
import System.Log.Handler.Simple
import "mtl" Control.Monad.Reader
import Data.Maybe
import Control.Applicative
import System.Directory
import Control.Monad.IfElse

import Propellor.Types
import Utility.Monad
import Utility.Env
import Utility.Process
import Utility.Exception

data MessageHandle
	= ConsoleMessageHandle
	| TextMessageHandle

mkMessageHandle :: IO MessageHandle
mkMessageHandle = do
	ifM (hIsTerminalDevice stdout <||> (isJust <$> getEnv "PROPELLOR_CONSOLE"))
		( return ConsoleMessageHandle
		, return TextMessageHandle
		)

forceConsole :: IO ()
forceConsole = void $ setEnv "PROPELLOR_CONSOLE" "1" True

isConsole :: MessageHandle -> Bool
isConsole ConsoleMessageHandle = True
isConsole _ = False

whenConsole :: MessageHandle -> IO () -> IO ()
whenConsole ConsoleMessageHandle a = a
whenConsole _ _ = return ()

-- | Shows a message while performing an action, with a colored status
-- display.
actionMessage :: (MonadIO m, ActionResult r) => Desc -> m r -> m r
actionMessage = actionMessage' Nothing

-- | Shows a message while performing an action on a specified host,
-- with a colored status display.
actionMessageOn :: (MonadIO m, ActionResult r) => HostName -> Desc -> m r -> m r
actionMessageOn = actionMessage' . Just

actionMessage' :: (MonadIO m, ActionResult r) => Maybe HostName -> Desc -> m r -> m r
actionMessage' mhn desc a = do
	h <- liftIO mkMessageHandle
	liftIO $ whenConsole h $ do
		setTitle $ "propellor: " ++ desc
		hFlush stdout

	r <- a

	liftIO $ do
		whenConsole h $
			setTitle "propellor: running"
		showhn h mhn
		putStr $ desc ++ " ... "
		let (msg, intensity, color) = getActionResult r
		colorLine h intensity color msg
		hFlush stdout

	return r
  where
	showhn _ Nothing = return ()
	showhn h (Just hn) = do
		whenConsole h $
			setSGR [SetColor Foreground Dull Cyan]
		putStr (hn ++ " ")
		whenConsole h $
			setSGR []

warningMessage :: MonadIO m => String -> m ()
warningMessage s = liftIO $ do
	h <- mkMessageHandle
	colorLine h Vivid Magenta $ "** warning: " ++ s

errorMessage :: MonadIO m => String -> m a
errorMessage s = liftIO $ do
	h <- mkMessageHandle
	colorLine h Vivid Red $ "** error: " ++ s
	error "Cannot continue!"

colorLine :: MessageHandle -> ColorIntensity -> Color -> String -> IO ()
colorLine h intensity color msg = do
	whenConsole h $
		setSGR [SetColor Foreground intensity color]
	putStr msg
	whenConsole h $
		setSGR []
	-- Note this comes after the color is reset, so that
	-- the color set and reset happen in the same line.
	putStrLn ""
	hFlush stdout

debug :: [String] -> IO ()
debug = debugM "propellor" . unwords

checkDebugMode :: IO ()
checkDebugMode = go =<< getEnv "PROPELLOR_DEBUG"
  where
	go (Just "1") = enableDebugMode
	go (Just _) = noop
	go Nothing = whenM (doesDirectoryExist ".git") $
		whenM (any (== "1") . lines <$> getgitconfig) $
			enableDebugMode
	getgitconfig = catchDefaultIO "" $
		readProcess "git" ["config", "propellor.debug"]

enableDebugMode :: IO ()
enableDebugMode = do
	f <- setFormatter
		<$> streamHandler stderr DEBUG
		<*> pure (simpleLogFormatter "[$time] $msg")
	updateGlobalLogger rootLoggerName $ 
		setLevel DEBUG .  setHandlers [f]
