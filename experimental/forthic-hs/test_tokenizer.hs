import Forthic.Tokenizer

sample1 = "# This is a comment\n\
          \ [ 1 2 3 ]"

sample2 = "# This is a comment"
sample3 = "[ 1 2"
sample4 = ": DOUBLE   DUP + ;"
sample5 = ": ITEMS   [ 1 2 3 ] ;"
sample6 = "{module"
sample7 = "{"
sample8 = "{\n: HOWDY   ;"
sample9 = "'''Now is the time\nfor all good men\nto come to the aid''' ["
sample10 = "'Now is the time\nfor all good men\nto come to the aid' ["
sample11 = "'''Now"
sample12 = "'Now"

main = putStrLn $ show $ tokens sample5
