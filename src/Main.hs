{-# LANGUAGE DeriveDataTypeable #-}

import            Control.Concurrent
import            Control.Exception
import            Control.Monad
import            Data.Binary
import            Data.ByteString (ByteString)
import qualified  Data.ByteString as BS
import qualified  Data.ByteString.Lazy as BL
import            Data.Char (digitToInt)
import qualified  Data.Vector as V
import            Data.Word (Word16)
import            System.Console.CmdArgs.Implicit
import            System.USB
import            Text.Printf

{- ########################################################################################## -}

_SET_REPORT :: Request
_SET_REPORT = 0x09

{- ########################################################################################## -}

data ApexCommand 
  = CmdEnableExtraKeys
  -- 0..8
  | CmdSetBrightness Word8
  -- 0 ->  125 Hz
  -- 1 ->  250 Hz
  -- 2 ->  500 Hz
  -- 3 -> 1000 Hz
  | CmdSetPollFrequency Word8
  | CmdSetColorProfile
  { colorSouth  :: Color
  , colorEast   :: Color
  , colorNorth  :: Color
  , colorWest   :: Color
  , colorLogo   :: Color
  } deriving (Show)

instance Binary ApexCommand where
  put (CmdEnableExtraKeys) = do
    put32 [0x02, 0x00, 0x02]

  put (CmdSetBrightness a) = do
    put32 [0x05, 0x01, a]

  put c@(CmdSetColorProfile {}) = do 
    put (0x07 :: Word8)
    put (0x00 :: Word8)
    put $ colorSouth c
    put $ colorEast c
    put $ colorNorth c
    put $ colorWest c
    put $ colorLogo c
    replicateM_ 10 $ put (0x00 :: Word8)

  put (CmdSetPollFrequency rate) = do
    put32 [0x04, 0x00, rate] 

put32 :: [Word8] -> Put
put32 words = do
  forM_ words $ put
  replicateM_ (32 - length words) $ put (0 :: Word8)

{- ########################################################################################## -}

data Color = Color Word8 Word8 Word8 Word8
  deriving (Data, Show, Typeable)

instance Binary Color where
  put (Color r g b a) = do
    put r
    put g
    put b
    put a

instance Read Color where
  readsPrec _ = \str -> case str of
    [r0, r1, g0, g1, b0, b1, ':', a0] ->
      [(Color (toComp2 r0 r1) (toComp2 g0 g1) (toComp2 b0 b1) (toAlpha a0), [])]
    [r0, r1, g0, g1, b0, b1] ->
      [(Color (toComp2 r0 r1) (toComp2 g0 g1) (toComp2 b0 b1) 8, [])]
    [r0, g0, b0, ':', a0] ->
      [(Color (toComp1 r0) (toComp1 g0) (toComp1 b0) (toAlpha a0), [])]
    [r0, g0, b0] ->
      [(Color (toComp1 r0) (toComp1 g0) (toComp1 b0) 8, [])]

toAlpha :: Char -> Word8
toAlpha alpha = case digitToInt alpha of 
  a | a >= 0 && a <= 8 
    -> fromIntegral a 
  otherwise 
    -> error "brightness must be between 0 and 8" 

toComp1 :: Char -> Word8
toComp1 a = fromIntegral $ (digitToInt a) * 0x10 + 0xF

toComp2 :: Char -> Char -> Word8
toComp2 a b = fromIntegral $ (digitToInt a) * 0x10 + (digitToInt b)

toFrequency :: Int -> Word8
toFrequency  125 = 0
toFrequency  250 = 1
toFrequency  500 = 2
toFrequency 1000 = 3
toFrequency    _ = error "frequency must be 125, 250, 500 or 1000"

{- ########################################################################################## -}

colorCyan   = Color 0x00 0xFF 0xFF 0x08
colorGreen  = Color 0x00 0xFF 0x00 0x08
colorPurple = Color 0xFF 0x00 0xFF 0x08
colorRed    = Color 0xFF 0x00 0x00 0x08
colorWhite  = Color 0xFF 0xFF 0xFF 0x08

cp0   = CmdSetColorProfile colorRed colorRed colorRed colorRed colorRed
cp1   = CmdSetColorProfile colorCyan colorGreen colorPurple colorRed colorWhite
cp2 c = CmdSetColorProfile c c c c c

{- ########################################################################################## -}

data ApexMode
  = ModeEnableExtraKeys
  | ModeSetBrightness 
  { argBrightness :: Word8 }
  | ModeSetColorProfile
  { argSouth  :: String
  , argEast   :: String
  , argNorth  :: String
  , argWest   :: String
  , argLogo   :: String
  } 
  | ModeSetPollFrequency 
  { argFrequency :: Int }
  deriving (Data, Show, Typeable)

apexArgs :: ApexMode
apexArgs = modes 
  [ modeEnableExtraKeys
  , modeSetBrightness
  , modeSetColorProfile
  , modeSetPollFrequency
  ] 
  &= program "apexctl" 
  &= summary "An utility for managing Apex/Apex [RAW] keyboard settings"

modeEnableExtraKeys :: ApexMode
modeEnableExtraKeys = ModeEnableExtraKeys 
  &= auto 
  &= help "Enable macro keys"
  &= name "init"

modeSetBrightness :: ApexMode 
modeSetBrightness = ModeSetBrightness
  { argBrightness = 8 &= argPos 0 &= typ "BRIGHTNESS"
  }
  &= help "Set brightness level (1..8)"
  &= name "br"

colorAnnot :: String -> String -> String -> String
{-# INLINE colorAnnot #-}
colorAnnot name0 name1 desc = "FF0000:8" 
  &= explicit 
  &= name name0 
  &= name name1 
  &= help desc
  &= typ "COLOR"

modeSetColorProfile :: ApexMode
modeSetColorProfile = ModeSetColorProfile
  { argSouth = colorAnnot "s" "south" "Color for South zone"
  , argEast  = colorAnnot "e" "east"  "Color for East zone"
  , argNorth = colorAnnot "n" "north" "Color for North zone"
  , argWest  = colorAnnot "w" "west"  "Color for West zone"
  , argLogo  = colorAnnot "l" "logo"  "Color for Logo zone"
  } 
  &= help (unlines 
    [ "Change color profile."
    , "Colors should be specified for all color zones."
    , "Otherwise unspecified colors will default to red" ])
  &= name "colors" 

modeSetPollFrequency :: ApexMode
modeSetPollFrequency = ModeSetPollFrequency
  { argFrequency = 1000 
    &= argPos 0 
    &= typ "FREQUENCY" 
  }
  &= help "Set polling frequency (125, 250, 500 or 1000)"
  &= name "freq" 

modeToCommand :: ApexMode -> ApexCommand

modeToCommand m@ModeEnableExtraKeys {} = CmdEnableExtraKeys

modeToCommand m@ModeSetBrightness {} = CmdSetBrightness (argBrightness m)

modeToCommand m@ModeSetColorProfile {} = CmdSetColorProfile
  { colorSouth  = read $ argSouth m
  , colorEast   = read $ argEast m
  , colorNorth  = read $ argNorth m
  , colorWest   = read $ argWest m
  , colorLogo   = read $ argLogo m
  }

modeToCommand m@ModeSetPollFrequency {} = CmdSetPollFrequency (toFrequency $ argFrequency m)

{- ########################################################################################## -}

apexCtl :: DeviceHandle -> ByteString -> IO ()
apexCtl devHndl d = do
  putStrLn "WRITING SET_REPORT"
  writeControlExact devHndl
    Class 
    ToInterface 
    _SET_REPORT 
    0x0200 
    0
    d
    noTimeout

apexCommand :: Binary b => DeviceHandle -> b -> IO ()
apexCommand devHndl b = do
  apexCtl devHndl $ toStrict1 $ encode b
  threadDelay 10000

apexRet :: DeviceHandle -> IO ()
apexRet devHndl = do
  (r, s) <- readControl devHndl 
    Class 
    ToInterface 
    _SET_REPORT 
    0x0200 
    0      
    32 noTimeout
  putStrLn $ hex r
  putStrLn $ show s

deviceFilter :: Word16 -> [Word16] -> Device -> IO Bool
deviceFilter venId prodIds dev = do
  devDesc <- getDeviceDesc dev
  return  $ deviceVendorId  devDesc == venId
         && any ((==) $ deviceProductId devDesc) prodIds

hex :: BS.ByteString -> String
hex = concatMap (printf "%02x") . BS.unpack

toStrict1 :: BL.ByteString -> BS.ByteString
toStrict1 = BS.concat . BL.toChunks

withDevice :: Word16 -> [Word16] -> (Device -> IO a) -> IO ()
withDevice venId prodIds hnd = do
  ctx   <- newCtx
  setDebug ctx PrintInfo
  devs  <- getDevices ctx
  devs1 <- V.filterM (deviceFilter venId prodIds) devs
  if V.null devs1 
    then return $ error "Device not found"
    else V.mapM_ hnd devs1

{- ########################################################################################## -}

main :: IO ()
main = do
  args <- cmdArgs apexArgs
  withDevice 0x1038 [0x1200, 0x1202] $ \dev -> do
  withDeviceHandle dev $ \devHndl ->
    withDetachedKernelDriver devHndl 0 $
    withClaimedInterface devHndl 0 $ do
      res <- try $ do
        apexCommand devHndl $ modeToCommand args
      case res of
        Left (SomeException a)  -> putStrLn $ show a
        Right _                 -> putStrLn "OK"

{- ########################################################################################## -}
