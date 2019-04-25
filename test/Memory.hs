import Weigh
import Protolude

main :: IO ()
main =
  mainWith $ do
    validateFunc "integers count 0"   count 0   (maxAllocs 0)
    validateFunc "integers count 1"   count 1   (maxAllocs 16)
    validateFunc "integers count 2"   count 2   (maxAllocs 32)
    validateFunc "integers count 3"   count 3   (maxAllocs 48)
    validateFunc "integers count 10"  count 10  (maxAllocs 160)
    validateFunc "integers count 100" count 100 (maxAllocs 1600)


count :: Int32 -> ()
count 0 = ()
count a = count (a - 1)
