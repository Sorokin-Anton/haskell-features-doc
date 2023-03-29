# Features


## Types that allow you to express the data flow

Haskell has a static type system, which means the type of every expression is
determined during the compilation, and no contradictions between the expected and
real type can happen during the runtime.

Sometimes, a static type system prevents you from writing reusable polymorphic
code and forces you to write some boilerplate, but Haskell's type system
is rich enough to describe the behavior of a polymorphic function.

This allows you not only to check this behavior during the compile time
but also to describe requirements for arguments, possible exceptions or
side effects, and other properties of your function.

---------------------------------
Each Haskell value has a type:

```haskell
'a'           :: Char
123           :: Int
(5, "five")   :: (Int, String)
isDigit       :: Char -> Bool           -- From Data.Char
hash          :: Hashable a => a -> Int -- From Data.Hashable (package "hashable")
```

You have to pass the right type of values to functions, or the compiler will reject the program:

```haskell
-- Type error
a = isDigit 1
```

`Hashable a` in the type of `hash` shows that you can hash a value of a restricted
amount of types. By going to the definition of a `Hashable` type class, or just by trying to
apply the `hash` to different values, we can conclude that e.g. `Int` and `String` are `Hashable`:

```haskell
-- This compiles
h1 = hash "str"
h2 = hash 2000
```

`Char -> Bool` is not `Hashable`, so we can not accidentally try to hash a function or
use functions as keys in `HashMap`:

```haskell
-- Type error
h3 = hash isDigit
```

## Declarative code built from composable blocks

Haskell's syntax and base libraries are designed for writing declarative code
We can implement a function by combining some other functions without loops
or mutable variables. This is less error-prone, and often much more compact.

Type inference allows using polymorphic functions without specifying
their types manually, which also increases the compactness of the code.

Also, Haskell is lazy, which saves us from memory consumption for creating
new objects during computation, and allows some functions to be effectively fused.

---------------------------------
This function calculates the euclidean norm of a vector
stored as a list of floating-point numbers:

```haskell
norm :: [Double] -> Double
norm v = sqrt (sum (map (^2) v))
```

The `(^2)` here is a partial application of the power operator `^`.
While `a ^ b` is `a` in power `b`, `(^b)` is a function that raises something to power `b`,
and `(a^)` is a function that raises `a` to a given power.

`map` is a function that applies a given function to each element of a list.
Generally, has type `(a -> b) -> [a] -> [b]`, and in this case it has type
`(Double -> Double) -> [Double] -> [Double]`.
Note that we haven't specified this
specialized type manually, it was inferred by the compiler.

While `map` returns a new list, we don't need to store it in memory, because we need
only its sum. Because of lazy evaluation, the compiler is not forced to evaluate the
subexpression `map (^2) v`, since we need only the sum of this list for further evaluation.
`map` and `sum` will be fused here, so this code will have the same performance as
a loop that goes through the list and adds up squares of its elements in a variable.

We can rewrite this function using application and composition operators:

```haskell
norm2 v = sqrt $ sum $ map (^2) v
norm3 = sqrt . sum . map (^2)
```

Note that `map` is partially applied in `norm3`. This is also useful if we want to do something
with a nested list:

```haskell
hashAllElements :: [[String]] -> [[Int]]
hashAllElements = map (map hash)
```

## Side effects are explicit

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
Its flagship compiler, GHC, comes with a high-performance parallel garbage collector and\
lightweight concurrency library containing several useful concurrency primitives and abstractions.

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
