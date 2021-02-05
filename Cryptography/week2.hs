import Data.Binary (encode)
import Data.Bits
import Data.Char (chr, ord)
import Data.Int
import Data.List.Split (chunksOf)
import Data.Maybe (fromJust)

import Text.Hex (encodeHex, decodeHex)
import qualified Data.Text as TXT
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import Crypto.Cipher.AES (AES128, AES256)
import Crypto.Cipher.Types (BlockCipher(..), Cipher(..),nullIV)
import Crypto.Error (throwCryptoError)

parseFromHex :: String -> BS.ByteString
parseFromHex cs = fromJust $ decodeHex $ TXT.pack cs

aesCycle128 :: BS.ByteString -> BS.ByteString -> BS.ByteString
aesCycle128 key plainText = ecbDecrypt ctx plainText
  where ctx :: AES128
        ctx = throwCryptoError $ cipherInit key

aesCycle256 :: BS.ByteString -> BS.ByteString -> BS.ByteString
aesCycle256 key plainText = ecbDecrypt ctx plainText
  where ctx :: AES256
        ctx = throwCryptoError $ cipherInit key

cdcD :: BS.ByteString -> BS.ByteString -> [BS.ByteString] -> String
cdcD _ _ [] = ""
cdcD key scamble (c:cs) = m ++ (cdcD key c cs)
  where k = aesCycle128 key (c)
        m = map (chr . fromEnum) $ BS.zipWith xor scamble k

fullcdcD :: (String, String) -> String
fullcdcD (key, cipherText) = cdcD key' iv msg'
  where key' = parseFromHex key
        (iv:msg') = map (parseFromHex) $ chunksOf (length key) cipherText
--------------------------------------------------------------------------------
incByteString :: BS.ByteString -> BS.ByteString
incByteString b = b

ctrD :: BS.ByteString -> BS.ByteString -> [BS.ByteString] -> String
ctrD _ _ [] = ""
ctrD key iv (c:cs) = m ++ (ctrD key (incByteString c) cs)
  where k = aesCycle128 key iv
        m = map (chr . fromEnum) $ BS.zipWith xor c k

fullctrD :: (String, String) -> String
fullctrD (key, cipherText) = ctrD key' iv msg
  where key' = parseFromHex key
        iv = parseFromHex $ take 16 cipherText
        msg = map (parseFromHex) $ chunksOf (length key) $ drop 16 cipherText
        -- (iv:msg') = map (parseFromHex) $ chunksOf (length key) cipherText

questions = [("140b41b22a29beb4061bda66b6747e14",
              "4ca00ff4c898d61e1edbf1800618fb2828a226d160dad07883d04e008a7897ee2e4b7465d5290d0c0e6c6822236e1daafb94ffe0c5da05d9476be028ad7c1d81"),
             ("140b41b22a29beb4061bda66b6747e14",
              "5b68629feb8606f9a6667670b75b38a5b4832d0f26e1ab7da33249de7d4afc48e713ac646ace36e872ad5fb8a512428a6e21364b0c374df45503473c5242a253"),
             ("36f18357be4dbd77f050515c73fcf9f2",
              "69dda8455c7dd4254bf353b773304eec0ec7702330098ce7f7520d1cbbb20fc388d1b0adb5054dbd7370849dbf0b88d393f252e764f1f5f7ad97ef79d59ce29f5f51eeca32eabedd9afa9329"),
             ("36f18357be4dbd77f050515c73fcf9f2",
              "770b80259ec33beb2561358a9f2dc617e46218c0a53cbeca695ae45faa8952aa0e311bde9d4e01726d3184c34451")]
exam = [("9f970f4e", "6068f0b1"),
        ("7c2822eb", "325032a9"),
        ("4af53267", "87a40cfa"),
        ("2d1cfa42", "eea6e3dd")]

main = do
  putStrLn "Week 2"

  putStrLn "Question 1"
  putStrLn $ show $ fullcdcD (questions!!0)
  putStrLn "Question 2"
  putStrLn $ show $ fullcdcD (questions!!1)
  putStrLn "Question 3"
  -- putStrLn $ show $ fullctrD (questions!!2)


  let (k, m) = (questions!!2)
  let msg = map (parseFromHex) $ chunksOf (length k) $ drop 16 m 

  let counter = 0 :: Data.Int.Int64
  let nonce = parseFromHex $ take 16 m
  let ctr = BSL.toStrict $ encode counter
  let iv = nonce <> ctr

  let key = parseFromHex k

  let q = aesCycle128 key iv
  let m0 = (msg!!0)
  let m = map (chr . fromEnum) $ BS.zipWith xor q m0
  putStrLn $ show m
  
  -- putStrLn $ show ctr
  -- putStrLn $ show nonce
  -- putStrLn $ show (nonce <> ctr)
  -- putStrLn iv
  -- putStrLn msg

  let examQ = map (\(x,y) -> BS.zipWith xor (parseFromHex x) (parseFromHex y)) exam
  putStrLn $ show examQ

  putStrLn $ show "Done"
