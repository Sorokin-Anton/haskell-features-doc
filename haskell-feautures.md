# Features

## Statically typed

Every expression in Haskell has a type which is determined at compile time.
All the types composed together by function application have to match up.
If they don't, the program will be rejected by the compiler.
Types become not only a form of guarantee,
but a language for expressing the construction of programs.

---------------------------------
All Haskell values have a type:

```haskell
char = 'a'    :: Char
int = 123     :: Int
fun = isDigit :: Char -> Bool
```

You have to pass the right type of values to functions, or the compiler will reject the program:

```haskell
-- Type error
a = isDigit 1
```

You can decode bytes into text:

```haskell
bytes = Crypto.Hash.SHA1.hash "hello" :: ByteString
text = decodeUtf8 bytes               :: Text
```

But you cannot decode Text, which is already a vector of Unicode points:

```haskell
-- Type error
doubleDecode = decodeUtf8 (decodeUtf8 bytes)
```

## Purely functional

Every function in Haskell is a function in the mathematical sense (i.e., "pure").
Even side-effecting IO operations are but a description of what to do, produced by pure code.
There are no statements or instructions, only expressions which cannot mutate variables
(local or global) nor access state like time or random numbers.

---------------------------------
The following function takes an integer and returns an integer.
By the type it cannot do any side-effects whatsoever, it cannot mutate any of its arguments.

```haskell
square :: Int -> Int
square x = x * x
```

The following string concatenation is okay:

```haskell
"Hello: " ++ "World!"
```

The following string concatenation is a type error:

```haskell
-- Type error
"Name: " ++ getLine
```

Because getLine has type IO String and not String, like "Name: " is. So by the type system you cannot mix and match purity with impurity.

## Type inference

You don't have to explicitly write out every type in a Haskell program. Types will be inferred by unifying every type bidirectionally. However, you can write out types if you choose, or ask the compiler to write them for you for handy documentation.

---------------------------------
This example has a type signature for every binding:

```haskell
main :: IO ()
main = do line :: String <- getLine
          print (parseDigit line)
  where parseDigit :: String -> Maybe Int
        parseDigit ((c :: Char) : _) =
          if isDigit c
             then Just (ord c - ord '0')
             else Nothing
```

But you can just write:

```haskell
main = do line <- getLine
          print (parseDigit line)
  where parseDigit (c : _) =
          if isDigit c
             then Just (ord c - ord '0')
             else Nothing
```

You can also use inference to avoid wasting time explaining what you want:

```haskell
do ss <- decode "[\"Hello!\",\"World!\"]"
   is <- decode "[1,2,3]"
   return (zipWith (\s i -> s ++ " " ++ show (i + 5)) ss is)
 => Just ["Hello! 6","World! 7"]
```

Types give a parser specification for free, the following input is not accepted:

```haskell
do ss <- decode "[1,2,3]"
   is <- decode "[null,null,null]"
   return (zipWith (\s i -> s ++ " " ++ show (i + 5)) ss is)
 => Nothing
```

## Concurrent

Haskell lends itself well to concurrent programming due to its explicit handling of effects.
Its flagship compiler, GHC, comes with a high-performance parallel garbage collector and
light-weight concurrency library
containing a number of useful concurrency primitives and abstractions.

---------------------------------
Easily launch threads and communicate with the standard library:

```haskell
main = do
  done <- newEmptyMVar
  forkIO (do putStrLn "I'm one thread!"
             putMVar done "Done!")
  second <- forkIO (do threadDelay 100000
                       putStrLn "I'm another thread!")
  killThread second
  msg <- takeMVar done
  putStrLn msg
```

Use an asynchronous API for threads:

```haskell
do a1 <- async (getURL url1)
  a2 <- async (getURL url2)
  page1 <- wait a1
  page2 <- wait a2
  ...
```

Atomic threading with software transactional memory:

```haskell
transfer :: Account -> Account -> Int -> IO ()
transfer from to amount =
  atomically (do deposit to amount
                 withdraw from amount)
```

Atomic transactions must be repeatable, so arbitrary `IO` is disabled in the type system:

```haskell
-- Type error
main = atomically (putStrLn "Hello!")
```

## Lazy

Functions don't evaluate their arguments.
This means that programs can compose together very well,
with the ability to write control constructs (such as if/else) just by writing normal functions.
The purity of Haskell code makes it easy to fuse chains of functions together,
allowing for performance benefits.

---------------------------------
Define control structures easily:

```haskell
when p m = if p then m else return ()
main = do args <- getArgs
          when (null args)
               (putStrLn "No args specified!")
```

If you notice a repeated expression pattern, like

```haskell
if c then t else False
```

you can give this a name, like

```haskell
and c t = if c then t else False
```

and then use it with the same effect as the original expression.

Get code re-use by composing lazy functions.
It's quite natural to express the `any` function by reusing the `map` and `or` functions:

```haskell
any :: (a -> Bool) -> [a] -> Bool
any p = or . map p
```

Reuse the recursion patterns in `map`, `filter`, `foldr`, etc.

## Packages

Open source contribution to Haskell is very active with a wide range of packages
available on the public package servers.

<!-- list of libs -->
