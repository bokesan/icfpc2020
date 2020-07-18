

newtype World = World Int

data MyIO a = MyIOSuccess a !World
            | MyIOError !Int String

type MyIOAction a = World -> MyIO a


newtype Handle = Handle Int

seqWorld :: a -> MyIOAction a
seqWorld v (World s) = MyIOSuccess v (World (s + 1))

myPutChar :: Handle -> Char -> MyIOAction ()
myPutChar h c = seqWorld ()

myGetChar :: Handle -> MyIOAction Char
myGetChar h = seqWorld 'a'


mySeqIO :: MyIOAction a -> (a -> MyIOAction b) -> MyIOAction b
mySeqIO a b = \w -> case a w of
                      MyIOError c s -> MyIOError c s
                      MyIOSuccess v w' -> b v w'

mySeqIO' :: MyIOAction a -> MyIOAction b -> MyIOAction b
mySeqIO' a b = mySeqIO a (const b)

myDoIOs :: [a -> MyIOAction a] -> a -> MyIOAction a
myDoIOs [] z = seqWorld z
myDoIOs (x:xs) z = mySeqIO (x z) (myDoIOs xs)

myDoIOs' :: [MyIOAction a] -> myIOAction a
myDoIOs' [x] = x
myDOIOs' (x:xs) = mySeqIO' x (myDoIOs' xs)

{- core versions

returnIO = Pack{0,2};
ioError  = Pack{1,2};

-- Built-in functions:


seqIO a b w = case a w of {
                <0> v w' -> b v w';
                err -> err
              };

seqIO' a b = seqIO a (const b);

doIO actions z = case actions of {
                   <0> -> returnIO z;
                   <1> x xs -> seqIO (x z) (doIO xs)
                 };

doIO' actions = case actions of {
                  <0> -> error "doIO': no actions";
                  <1> a1 r -> case r of {
                                <0> -> a1;
                                _   -> seqIO' a1 (doIO' r)
                              }
                }
-}