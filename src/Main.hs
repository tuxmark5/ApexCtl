import            Control.Concurrent
import            Control.Exception
import            Control.Monad
import            Data.Binary
import            Data.ByteString (ByteString)
import qualified  Data.ByteString as BS
import qualified  Data.ByteString.Lazy as BL
import qualified  Data.Vector as V
import            Data.Word (Word16)
import            System.USB
import            Text.Printf

_SET_REPORT :: Request
_SET_REPORT = 0x09

{-
0040  16 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00   ........ ........
0050  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00   ........ ........ 

0040  07 00 0e f1 0e 08 ef fa  60 08 ef fa 60 08 ef fa   ........ `...`...
0050  60 08 ef fa 60 08 00 00  00 00 00 00 00 00 00 00   `...`... ........
0040  07 00 00 ff ff 08 00 ff  00 08 ff 00 ff 08 ff 00   ........ ........
0050  00 08 ff ff ff 08 00 00  00 00 00 00 00 00 00 00   ........ ........

07 00
00 ff ff 08 
00 ff 00 08 
ff 00 ff 08 
ff 00 00 08 
ff ff ff 08 
00 00 00 00 
00 00 00 00 
00 00

-}

{- ########################################################################################## -}

data Color = Color Word8 Word8 Word8 Word8

data SetColorProfile = SetColorProfile
  { colorSouth  :: Color
  , colorEast   :: Color
  , colorNorth  :: Color
  , colorWest   :: Color
  , colorLogo   :: Color
  }

instance Binary Color where
  put (Color r g b a) = do
    put r
    put g
    put b
    put a

instance Binary SetColorProfile where
  put cp = do
    put (0x07 :: Word8)
    put (0x00 :: Word8)
    put $ colorSouth cp
    put $ colorEast cp
    put $ colorNorth cp
    put $ colorWest cp
    put $ colorLogo cp
    replicateM_ 10 $ put (0x00 :: Word8)

{- ########################################################################################## -}

colorCyan   = Color 0x00 0xFF 0xFF 0x08
colorGreen  = Color 0x00 0xFF 0x00 0x08
colorPurple = Color 0xFF 0x00 0xFF 0x08
colorRed    = Color 0xFF 0x00 0x00 0x08
colorWhite  = Color 0xFF 0xFF 0xFF 0x08

cp0   = SetColorProfile colorRed colorRed colorRed colorRed colorRed
cp1   = SetColorProfile colorCyan colorGreen colorPurple colorRed colorWhite
cp2 c = SetColorProfile c c c c c

{- ########################################################################################## -}

cdataInit :: BS.ByteString
cdataInit = BS.pack
  [ 0x02, 0x00, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00
  , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
  , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
  , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
  ]

cdata :: BS.ByteString
cdata = BS.pack
  [ 0x16, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
  , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
  , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
  , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
  ]

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

apexEnableExtraKeys :: DeviceHandle -> IO ()
apexEnableExtraKeys devHndl = apexCtl devHndl cdataInit

apexSetColorProfile :: DeviceHandle -> SetColorProfile -> IO ()
apexSetColorProfile devHndl cp = do
  apexCtl devHndl $ toStrict1 $ encode cp
  threadDelay 10000

{- ########################################################################################## -}

main :: IO ()
main = do
  withDevice 0x1038 [0x1200,0x1202] $ \dev -> do
  withDeviceHandle dev $ \devHndl ->
    withDetachedKernelDriver devHndl 0 $
    withClaimedInterface devHndl 0 $ do
      res <- try $ do
        apexEnableExtraKeys devHndl
        --apexSetColorProfile devHndl cp0
      case res of
        Left (SomeException a)  -> putStrLn $ show a
        Right _                 -> putStrLn "OK"

{- ########################################################################################## -}
