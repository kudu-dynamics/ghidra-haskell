module Ghidra.State where

import Ghidra.Prelude hiding (force, get)

import qualified Language.Java as Java
import Ghidra.Util (iteratorToList, maybeNullCall, suppressOut, tryJVM, getDomainObject)
import qualified Ghidra.Types as J
import qualified Data.BinaryAnalysis as BA
import qualified Foreign.JNI as JNI
import qualified Data.Text as Text


data GhidraState = GhidraState
  { taskMonitor :: J.TaskMonitor
  , program :: J.ProgramDB
  , flatProgramAPI :: J.FlatProgramAPI
  , flatDecompilerAPI :: J.FlatDecompilerAPI
  } deriving (Eq, Ord, Show, Generic)

data OpenDatabaseOptions = OpenDatabaseOptions
  { compiler :: Maybe Text
  , language :: Maybe Text
  , quiet :: Bool
  } deriving (Eq, Ord, Show, Generic)

defaultOpenDatabaseOptions :: OpenDatabaseOptions
defaultOpenDatabaseOptions = OpenDatabaseOptions Nothing Nothing True

getProgram :: GhidraState -> IO J.ProgramDB
getProgram = return . view #program

getTaskMonitor :: GhidraState -> IO J.TaskMonitor
getTaskMonitor = return . view #taskMonitor

getFlatDecompilerAPI :: GhidraState -> IO J.FlatDecompilerAPI
getFlatDecompilerAPI = return . view #flatDecompilerAPI

getLang :: Text -> IO (Maybe J.Language)
getLang lang = do
  langProvider :: J.SleighLanguageProvider <- Java.new >>= JNI.newGlobalRef
  maybeNullCall (Java.reflect lang >>= Java.new >>= JNI.newGlobalRef) >>= \case
    Nothing -> return Nothing
    Just (langId :: J.LanguageID) -> do
      maybeNullCall $ Java.call langProvider "getLanguage" langId >>= JNI.newGlobalRef

getCSpec :: J.Language -> Maybe Text -> IO J.CompilerSpec
getCSpec lang Nothing = Java.call lang "getDefaultCompilerSpec"
getCSpec lang (Just compilerName) = do
  compSpecId :: J.CompilerSpecID <- Java.reflect compilerName >>= Java.new >>= JNI.newGlobalRef
  Java.call lang "getCompilerSpecByID" compSpecId >>= JNI.newGlobalRef

hasBeenAnalyzed :: GhidraState -> IO Bool
hasBeenAnalyzed gs = do
  programInfoField :: J.String <- Java.getStaticField "ghidra.program.model.listing.Program" "PROGRAM_INFO" >>= JNI.newGlobalRef
  analyzedField :: J.String <- Java.getStaticField "ghidra.program.model.listing.Program" "ANALYZED_OPTION_NAME" >>= Java.reify >>= JNI.newGlobalRef
  prgOptions :: J.Options <- Java.call (gs ^. #program) "getOptions" programInfoField >>= JNI.newGlobalRef
  Java.call prgOptions "getBoolean" analyzedField False


data OpenDatabaseError
  = CouldNotFindFile Text
  | CouldNotFindLang Text
  | ImportByUsingBestGuessError Text
  | ImportByLookingForLcsError Text
  deriving (Eq, Ord, Show, Generic)

-- | Opens binary as Ghidra database.
openDatabase'
  :: OpenDatabaseOptions
  -> FilePath
  -> IO (Either OpenDatabaseError GhidraState)
openDatabase' opts fp = do
  config :: J.HeadlessGhidraApplicationConfiguration <- Java.new >>= JNI.newGlobalRef
  layout :: J.GhidraJarApplicationLayout <- Java.new >>= JNI.newGlobalRef
  _ :: () <- bool identity suppressOut (opts ^. #quiet) $ do
    Java.callStatic "ghidra.framework.Application" "isInitialized" >>= \case
      True -> return ()
      False -> Java.callStatic "ghidra.framework.Application" "initializeApplication" (coerce layout :: J.ApplicationLayout) (coerce config :: J.ApplicationConfiguration)
  consumer :: J.Object <- Java.new >>= JNI.newGlobalRef
  messageLog :: J.MessageLog <- Java.new >>= JNI.newGlobalRef
  tm :: J.TaskMonitor <- Java.getStaticField "ghidra.util.task.TaskMonitor" "DUMMY" >>= JNI.newGlobalRef
  file :: J.File <- Java.reflect (cs fp :: Text) >>= Java.new >>= JNI.newGlobalRef
  runExceptT $ do
    results :: J.LoadResults J.Program <- case opts ^. #language of
      Nothing ->
        liftEitherM . fmap (first ImportByUsingBestGuessError) . tryJVM
        $ Java.callStatic
          "ghidra.app.util.importer.AutoImporter"
          "importByUsingBestGuess"
          file
          (Java.jnull :: J.Project)
          (Java.jnull :: J.String)
          consumer
          messageLog
          tm
          >>= JNI.newGlobalRef
      Just lang -> do
        lang' <- liftMaybeM (CouldNotFindLang lang) $ getLang lang
        cspec <- liftIO . getCSpec lang' $ opts ^. #compiler
        liftEitherM . fmap (first ImportByLookingForLcsError) . tryJVM
          $ Java.callStatic
            "ghidra.app.util.importer.AutoImporter"
            "importByLookingForLcs"
            file
            (Java.jnull :: J.Project)
            (Java.jnull :: J.String)
            lang'
            cspec
            consumer
            messageLog
            tm
            >>= JNI.newGlobalRef
    liftIO $ do
      resultsIter :: J.Iterator (J.Loaded J.Program) <- Java.call results "iterator" >>= JNI.newGlobalRef
      loadedProgs :: [J.Loaded J.Program] <- iteratorToList resultsIter
      -- Presently, this object implementing the `Program` interface is always
      -- a `ProgramDB` class instance.
      prgs :: [J.Program] <- traverse getDomainObject loadedProgs
      let prg = head prgs
      flatApi :: J.FlatProgramAPI <- Java.new prg >>= JNI.newGlobalRef
      flatDecApi :: J.FlatDecompilerAPI <- Java.new flatApi >>= JNI.newGlobalRef
      return $ GhidraState tm (coerce prg :: J.ProgramDB) flatApi flatDecApi

openDatabase :: FilePath -> IO (Either OpenDatabaseError GhidraState)
openDatabase = openDatabase' defaultOpenDatabaseOptions

openDatabase_ :: FilePath -> IO GhidraState
openDatabase_ = fmap unsafeFromRight . openDatabase


data AnalyzeOptions = AnalyzeOptions
  { force :: Bool
  , quiet :: Bool
  } deriving (Eq, Ord, Show, Generic)

defaultAnalyzeOptions :: AnalyzeOptions
defaultAnalyzeOptions = AnalyzeOptions False True

analyze' :: AnalyzeOptions -> GhidraState -> IO ()
analyze' opts gs = do
  alreadyAnalyzed <- hasBeenAnalyzed gs
  when (not alreadyAnalyzed || (opts ^. #force)) $
    bool identity suppressOut (opts ^. #quiet) $ do
      let prg = gs ^. #program
      txId :: Int32 <- Java.call prg "startTransaction" =<< Java.reflect ("Analysis" :: Text)
      _ :: () <- Java.call (gs ^. #flatProgramAPI) "analyzeAll" (coerce prg :: J.Program)
      _ :: () <- Java.callStatic "ghidra.program.util.GhidraProgramUtilities" "markProgramAnalyzed" (coerce prg :: J.Program)
      _ :: () <- Java.call prg "endTransaction" txId True
      return ()

analyze :: GhidraState -> IO ()
analyze = analyze' defaultAnalyzeOptions

getListing :: GhidraState -> IO J.Listing
getListing gs = do
  prg <- getProgram gs
  Java.call prg "getListing" >>= JNI.newGlobalRef

-- | Adds address to image base.
-- Only use this with PIE binaries.
mkAddressBased :: GhidraState -> BA.Address -> IO J.Address
mkAddressBased gs addr = do
  prg <- getProgram gs
  baseAddr :: J.Address <- Java.call prg "getImageBase" >>= JNI.newGlobalRef
  Java.call baseAddr "add" (fromIntegral addr :: Int64)

-- | Makes a new address
mkAddress :: GhidraState -> BA.Address -> IO J.Address
mkAddress gs addr = do
  prg <- getProgram gs
  baseAddr :: J.Address <- Java.call prg "getImageBase" >>= JNI.newGlobalRef
  Java.call baseAddr "getNewAddress" (fromIntegral addr :: Int64)

saveDatabase :: GhidraState -> FilePath -> IO ()
saveDatabase gs fp = do
  let prg = gs ^. #program
  () <- Java.call prg "updateMetadata"
  jstringFilePath <- Java.reflect . Text.pack $ fp
  file :: J.File <- Java.new jstringFilePath
  () <- Java.call (coerce prg :: J.DomainObjectAdapterDB) "saveToPackedFile" file (gs ^. #taskMonitor)
  return ()
