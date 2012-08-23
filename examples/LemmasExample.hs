import System.IO (getContents)
import Codec.Binary.UTF8.String (encodeString, decodeString)
import Data.Maybe (fromJust)
import Morfeusz

-- | Read word from stdin and analyse it with Morfeusz. 
main = do
    -- Set Morfeusz encoding to UTF-8
    setEncoding utf8    

    -- Get unicode encoded word from stdin
    word <- getContents

    -- Encode word in UTF-8, send to analysis and print retrieved lemmas.
    analyse (encodeString word) >>= mapM put
      where
        put = putStrLn . decodeString . fromJust . lemma
