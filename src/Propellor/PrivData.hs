{-# LANGUAGE PackageImports #-}
{-# LANGUAGE FlexibleContexts #-}

module Propellor.PrivData (
	withPrivData,
	withSomePrivData,
	addPrivData,
	setPrivData,
	dumpPrivData,
	editPrivData,
	filterPrivData,
	listPrivDataFields,
	makePrivDataDir,
	decryptPrivData,
	PrivMap,
) where

import Control.Applicative
import System.IO
import System.Directory
import Data.Maybe
import Data.Monoid
import Data.List
import Control.Monad
import Control.Monad.IfElse
import "mtl" Control.Monad.Reader
import qualified Data.Map as M
import qualified Data.Set as S

import Propellor.Types
import Propellor.Types.PrivData
import Propellor.Message
import Propellor.Info
import Propellor.Gpg
import Propellor.PrivData.Paths
import Utility.Monad
import Utility.PartialPrelude
import Utility.Exception
import Utility.Tmp
import Utility.SafeCommand
import Utility.Misc
import Utility.FileMode
import Utility.Env
import Utility.Table

-- | Allows a Property to access the value of a specific PrivDataField,
-- for use in a specific Context or HostContext.
--
-- Example use:
--
-- > withPrivData (PrivFile pemfile) (Context "joeyh.name") $ \getdata ->
-- >     property "joeyh.name ssl cert" $ getdata $ \privdata ->
-- >       liftIO $ writeFile pemfile privdata
-- >   where pemfile = "/etc/ssl/certs/web.pem"
-- 
-- Note that if the value is not available, the action is not run
-- and instead it prints a message to help the user make the necessary
-- private data available.
--
-- The resulting Property includes Info about the PrivDataField
-- being used, which is necessary to ensure that the privdata is sent to
-- the remote host by propellor.
withPrivData
	:: (IsContext c, IsPrivDataSource s, IsProp (Property i))
	=> s
	-> c
	-> (((PrivData -> Propellor Result) -> Propellor Result) -> Property i)
	-> Property HasInfo
withPrivData s = withPrivData' snd [s]

-- Like withPrivData, but here any one of a list of PrivDataFields can be used.
withSomePrivData
	:: (IsContext c, IsPrivDataSource s, IsProp (Property i))
	=> [s]
	-> c
	-> ((((PrivDataField, PrivData) -> Propellor Result) -> Propellor Result) -> Property i)
	-> Property HasInfo
withSomePrivData = withPrivData' id

withPrivData' 
	:: (IsContext c, IsPrivDataSource s, IsProp (Property i))
	=> ((PrivDataField, PrivData) -> v)
	-> [s]
	-> c
	-> (((v -> Propellor Result) -> Propellor Result) -> Property i)
	-> Property HasInfo
withPrivData' feed srclist c mkprop = addinfo $ mkprop $ \a ->
	maybe missing (a . feed) =<< getM get fieldlist
  where
  	get field = do
		context <- mkHostContext hc <$> asks hostName
		maybe Nothing (\privdata -> Just (field, privdata))
			<$> liftIO (getLocalPrivData field context)
	missing = do
		Context cname <- mkHostContext hc <$> asks hostName
		warningMessage $ "Missing privdata " ++ intercalate " or " fieldnames ++ " (for " ++ cname ++ ")"
		liftIO $ putStrLn $ "Fix this by running:"
		liftIO $ showSet $
			map (\s -> (privDataField s, Context cname, describePrivDataSource s)) srclist
		return FailedChange
	addinfo p = infoProperty
		(propertyDesc p)
		(propertySatisfy p)
		(propertyInfo p <> mempty { _privData = privset })
		(propertyChildren p)
	privset = S.fromList $ map (\s -> (privDataField s, describePrivDataSource s, hc)) srclist
	fieldnames = map show fieldlist
	fieldlist = map privDataField srclist
	hc = asHostContext c

showSet :: [(PrivDataField, Context, Maybe PrivDataSourceDesc)] -> IO ()
showSet l = forM_ l $ \(f, Context c, md) -> do
	putStrLn $ "  propellor --set '" ++ show f ++ "' '" ++ c ++ "' \\"
	maybe noop (\d -> putStrLn $ "    " ++ d) md
	putStrLn ""

addPrivData :: (PrivDataField, Maybe PrivDataSourceDesc, HostContext) -> Property HasInfo
addPrivData v = pureInfoProperty (show v) $
	mempty { _privData = S.singleton v }

{- Gets the requested field's value, in the specified context if it's
 - available, from the host's local privdata cache. -}
getLocalPrivData :: PrivDataField -> Context -> IO (Maybe PrivData)
getLocalPrivData field context =
	getPrivData field context . fromMaybe M.empty <$> localcache
  where
	localcache = catchDefaultIO Nothing $ readish <$> readFile privDataLocal

type PrivMap = M.Map (PrivDataField, Context) PrivData

-- | Get only the set of PrivData that the Host's Info says it uses.
filterPrivData :: Host -> PrivMap -> PrivMap
filterPrivData host = M.filterWithKey (\k _v -> S.member k used)
  where
	used = S.map (\(f, _, c) -> (f, mkHostContext c (hostName host))) $
		_privData $ hostInfo host

getPrivData :: PrivDataField -> Context -> PrivMap -> Maybe PrivData
getPrivData field context = M.lookup (field, context)

setPrivData :: PrivDataField -> Context -> IO ()
setPrivData field context = do
	putStrLn "Enter private data on stdin; ctrl-D when done:"
	setPrivDataTo field context =<< hGetContentsStrict stdin

dumpPrivData :: PrivDataField -> Context -> IO ()
dumpPrivData field context =
	maybe (error "Requested privdata is not set.") putStrLn
		=<< (getPrivData field context <$> decryptPrivData)

editPrivData :: PrivDataField -> Context -> IO ()
editPrivData field context = do
	v <- getPrivData field context <$> decryptPrivData
	v' <- withTmpFile "propellorXXXX" $ \f h -> do
		hClose h
		maybe noop (writeFileProtected f) v
		editor <- getEnvDefault "EDITOR" "vi"
		unlessM (boolSystem editor [File f]) $
			error "Editor failed; aborting."
		readFile f
	setPrivDataTo field context v'

listPrivDataFields :: [Host] -> IO ()
listPrivDataFields hosts = do
	m <- decryptPrivData
	
	section "Currently set data:"
	showtable $ map mkrow (M.keys m)
	let missing = M.keys $ M.difference wantedmap m
	
	unless (null missing) $ do
		section "Missing data that would be used if set:"
		showtable $ map mkrow missing

		section "How to set missing data:"
		showSet $ map (\(f, c) -> (f, c, join $ M.lookup (f, c) descmap)) missing
  where
	header = ["Field", "Context", "Used by"]
	mkrow k@(field, (Context context)) =
		[ shellEscape $ show field
		, shellEscape context
		, intercalate ", " $ sort $ fromMaybe [] $ M.lookup k usedby
		]
	mkhostmap host mkv = M.fromList $ map (\(f, d, c) -> ((f, mkHostContext c (hostName host)), mkv d)) $
		S.toList $ _privData $ hostInfo host
	usedby = M.unionsWith (++) $ map (\h -> mkhostmap h $ const $ [hostName h]) hosts
	wantedmap = M.fromList $ zip (M.keys usedby) (repeat "")
	descmap = M.unions $ map (\h -> mkhostmap h id) hosts
	section desc = putStrLn $ "\n" ++ desc
	showtable rows = do
		putStr $ unlines $ formatTable $ tableWithHeader header rows

setPrivDataTo :: PrivDataField -> Context -> PrivData -> IO ()
setPrivDataTo field context value = do
	makePrivDataDir
	m <- decryptPrivData
	let m' = M.insert (field, context) (chomp value) m
	gpgEncrypt privDataFile (show m')
	putStrLn "Private data set."
	void $ boolSystem "git" [Param "add", File privDataFile]
  where
	chomp s
		| end s == "\n" = chomp (beginning s)
		| otherwise = s

decryptPrivData :: IO PrivMap
decryptPrivData = fromMaybe M.empty . readish <$> gpgDecrypt privDataFile

makePrivDataDir :: IO ()
makePrivDataDir = createDirectoryIfMissing False privDataDir
