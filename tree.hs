import Codec.Picture  (generateImage, writePng)
import Codec.Picture.Types (PixelRGB8(..))
import Data.Word      (Word8)
import Data.Complex   (Complex(..), magnitude)


render :: Int -> Int -> PixelRGB8
render x y = PixelRGB8 (fromIntegral x) 0 (fromIntegral (255 - x))


main :: IO ()
main = writePng "tree.png" $ generateImage render 256 256