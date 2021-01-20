import Data.Bits
import Data.Char (chr, ord)
import Data.Int
import Data.List.Split (chunksOf)
import Data.Maybe (fromJust)

import Text.Hex (encodeHex, decodeHex)
import qualified Data.Text as TXT
import qualified Data.ByteString as BS

import Crypto.Cipher.AES (AES128)
import Crypto.Cipher.Types (BlockCipher(..), Cipher(..),nullIV)
import Crypto.Error (throwCryptoError)

parseFromHex :: String -> BS.ByteString
parseFromHex cs = fromJust $ decodeHex $ TXT.pack cs

aesCycle :: BS.ByteString -> BS.ByteString -> BS.ByteString
aesCycle key plainText = ecbDecrypt ctx plainText
  where ctx :: AES128
        ctx = throwCryptoError $ cipherInit key

cdcD :: BS.ByteString -> BS.ByteString -> [BS.ByteString] -> String
cdcD _ _ [] = ""
cdcD key scamble (c:cs) = m ++ (cdcD key c cs)
  where k = aesCycle key (c)
        m = map (chr . fromEnum) $ BS.zipWith xor scamble k

fullcdcD :: (String, String) -> String
fullcdcD (key, cipherText) = cdcD key' iv msg'
  where key' = parseFromHex key
        (iv:msg') = map (parseFromHex) $ chunksOf (length key) cipherText

questions = [("140b41b22a29beb4061bda66b6747e14",
              "4ca00ff4c898d61e1edbf1800618fb2828a226d160dad07883d04e008a7897ee2e4b7465d5290d0c0e6c6822236e1daafb94ffe0c5da05d9476be028ad7c1d81"),
             ("140b41b22a29beb4061bda66b6747e14",
              "5b68629feb8606f9a6667670b75b38a5b4832d0f26e1ab7da33249de7d4afc48e713ac646ace36e872ad5fb8a512428a6e21364b0c374df45503473c5242a253"),
             ("36f18357be4dbd77f050515c73fcf9f2",
              "69dda8455c7dd4254bf353b773304eec0ec7702330098ce7f7520d1cbbb20fc388d1b0adb5054dbd7370849dbf0b88d393f252e764f1f5f7ad97ef79d59ce29f5f51eeca32eabedd9afa9329")]
main = do
  putStrLn "Week 2"

  putStrLn "Question 1"
  putStrLn $ show $ fullcdcD (questions!!0)
  putStrLn "Question 2"
  putStrLn $ show $ fullcdcD (questions!!1)

  let (k, msg) = (questions!!2)
  let counter = 0 :: Data.Int.Int64
  putStrLn k
  putStrLn msg

  putStrLn $ show "Done"
