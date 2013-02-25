% Discovering Lenses
% Eric Mertens

Reimplementing mapM
===================

 <!--

> import Control.Applicative hiding (Const)
> import Data.Bits
> import Data.Monoid
> import Data.Map (Map)
> import qualified Data.Map as Map

-->


We can implement the side-effecting list update function with
slightly less restrictive constraints:

> mapA :: Applicative f => (a -> f b) -> [a] -> f [b]
> mapA _ []     = pure []
> mapA f (x:xs) = (:) <$> f x <*> mapA f xs

Let's pick an "interesting" update function to use in our examples.

> update1 :: Int -> IO Int
> update1 x = do let x' = x + 1
>                putStrLn (show x ++ " -> " ++ show x')
>                return x'

> example1 :: IO [Int]
> example1 = mapA update1 [1..3]
> -- 1 -> 2 , 2 -> 3 , 3 -> 4 , [2,3,4]

Update functions for tuples
===========================

Side-effecting update functions are useful for more types than lists.
Updating many values (like list) require `Applicative` to link the updates
together. Updating a single value requires only `Functor`:

> fstF :: Functor f => (a -> f b) -> (a,c) -> f (b,c)
> fstF f (a,c) = (\b -> (b,c)) <$> f a

> sndF :: Functor f => (b -> f c) -> (a,b) -> f (a,c)
> sndF f (a,b) = (\c -> (a,c)) <$> f b

> example2 :: IO (Int,Char)
> example2 = fstF update1 (10,'Z')
> -- 10 -> 11 , (11,'Z')

Naming the update pattern
=========================

All of these functions will follow the same type pattern:

> type LensLike f s t a b = (a -> f b) -> s -> f t

This allows us to "simplify" our types so far:

< mapA :: Applicative f => LensLike f [a] [b] a b
< fstF :: Functor f => LensLike f (a,c) (b,c) a b
< sndF :: Functor f => LensLike f (a,b) (a,c) b c

Capturing the one-place update pattern
======================================

< fstF :: Functor f => LensLike f (a,c) (b,c) a b
< fstF f (a,c) = (\b -> (b,c)) <$> f a

> fstF' f p = insert p <$> f (extract p)
>   where extract (a, _)  = a
>         insert  (_,b) a = (a,b)

Capture pattern by parametrizing on `extract` and `update`:

> lens :: Functor f => (s -> a) -> (s -> b -> t) ->
>                      LensLike f s t a b
> lens extract insert modify s
>   = insert s <$> modify (extract s)

Update functions for sum types
==============================

We can also write update functions on sum types.

> leftA :: Applicative f =>
>    LensLike f (Either a c) (Either b c) a b
> leftA f (Left  a) = Left <$> f a
> leftA _ (Right c) = pure (Right c)

> rightA :: Applicative f =>
>    LensLike f (Either a b) (Either a c) b c
> rightA _ (Left  a) = pure (Left a)
> rightA f (Right b) = Right <$> f b

> maybeA :: Applicative f =>
>    LensLike f (Maybe a) (Maybe b) a b
> maybeA _ Nothing  = pure Nothing
> maybeA f (Just x) = Just <$> f x

Sum type examples
=================

Let's see leftA in action.

> example3 :: IO (Either Int Char)
> example3 = leftA update1 (Left 10)
> -- 10 -> 11
> -- Left 11

> example4 :: IO (Either Int Char)
> example4 = leftA update1 (Right 'E')
> -- Right 'E'

Composing update functions
==========================

Side-effecting update functions compose nicely:

< mapA :: Applicative f => LensLike f [a] [b] a b
< fstF :: Functor f => LensLike f (a,c) (b,c) a b

> mapFstA :: Applicative f =>
>    LensLike f [(a,c)] [(b,c)] a b
> mapFstA = mapA . fstF

> example5 :: IO [(Int,Char)]
> example5 = mapFstA update1 [(10,'A'),(20,'B')]
> -- 10 -> 11 , 20 -> 21 , [(11,'A'),(21,'B')]

Composing update functions (again)
==================================

By composing "leftA" and "mapA" we get a side-effecting update function
for the list elements of "Either [a] b":

< leftA :: Applicative f =>
<    LensLike f (Either a c) (Either b c) a b
< mapA  :: Applicative f => (a -> f b) -> [a] -> f [b]

> leftMapA :: Applicative f =>
>    LensLike f (Either [a] c) (Either [b] c) a b
> leftMapA = leftA . mapA

> example6 :: IO (Either [Int] Char)
> example6 = leftMapA update1 (Left [10,20])
> -- 10 -> 11 , 20 -> 21 , Left [11, 21]

> example7 :: IO (Either [Int] Char)
> example7 = leftMapA update1 (Right 'K') -- Right 'K'

Pure Updates
============

Often you don't want to perform a side-effect when updating a value. Let's
define an applicative functor for this case and a wrapper function.

> newtype Id a = Id { runId :: a }
>
> instance Functor Id where
>   fmap f (Id x) = Id (f x)
>
> instance Applicative Id where
>   pure          = Id
>   Id f <*> Id x = Id (f x)

> over :: LensLike Id s t a b ->
>         (a -> b) -> s -> t
> over u f x = runId (u (Id . f) x)

Pure update examples
====================

We can see our pure update function in action:

< over fstF :: (a -> b) -> (a,c) -> (b,c)

> example8 :: (Int, Char)
> example8 = over fstF (*2) (40, 'P')
> -- (80, 'P')

< over (mapA . leftA) ::
<   (a -> b) -> [Either a c] -> [Either b c]

> example9 :: [Either Int Char]
> example9 = over (mapA . leftA) (+5)
>                 [Left 0, Left 10, Right 'Q']
> -- [Left 5, Left 15, Right 'Q']

Collection as a side-effect
===========================

Instead of updating a structure, we can use side-effects to read values.

> newtype Const r a = Const { runConst :: r }
>
> instance Functor (Const r) where
>    fmap _ (Const r) = Const r
>
> instance Monoid r => Applicative (Const r) where
>    pure _              = Const mempty
>    Const a <*> Const b = Const (a <> b)

> view :: LensLike (Const a) s t a b -> s -> a
> view u x = runConst (u Const x)

> example10 :: Int
> example10 = view fstF (10,20) -- 10

Collecting multiple values
==========================

Typically, we don't want to merge all of the values we are collecting
into one. We can use the list monoid to help.

> toListOf :: LensLike (Const [a]) s t a b -> s -> [a]
> toListOf u s = runConst (u (\a -> Const [a]) s)

> example11 :: [Int]
> example11 = toListOf (mapA . fstF) [(10,'A'),(20,'B')]
> -- [10,20]

> example12 :: [Int]
> example12 = toListOf (leftA . sndF) (Right False)
> -- []

Working with computed values
============================

We can create lenses to work on computed values.

> bitF :: (Bits a, Functor f) =>
>     Int -> LensLike f a a Bool Bool
> bitF i = lens (flip testBit i) assignBit
>   where
>   assignBit n True  = setBit   n i
>   assignBit n False = clearBit n i

> example13 :: Bool
> example13 = view (bitF 2) (10 :: Int) -- False

> example14 :: Int
> example14 = over (bitF 2) not 10 -- 14

Working with computed values (again)
====================================

Another example is a family of lenses for looking inside a Map.

> at :: (Ord k, Functor f) => k ->
>     LensLike f (Map k v) (Map k v) (Maybe v) (Maybe v)
> at k = lens (Map.lookup k) assign
>   where
>   assign m Nothing  = Map.delete k m
>   assign m (Just v) = Map.insert k v m

> example15 :: Map String Int
> example15 = over (at "k" . maybeA) (*10) m
>   where
>   m = Map.fromList [("k", 10), ("x", 20)]
> -- fromList [("k",100),("x",20)]

lens Package Names
==================

These concepts are all reimplementations of types and functions from
the "lens" package.

< type Mutator = Id
<
< type Accessor = Const
<
< type Lens s t a b =
<   forall f. Functor f => (a -> f b) -> s -> f t
<
< type Traversal s t a b =
<   forall f. Applicative f => (a -> f b) -> s -> f t

What else?
==========

You've seen the basic functionality, but what's next?

* Prisms
    + constructors and traversals for sum types

* Isos
    + reversable constructors and traversals

* Folds
    + traversals that lose the original structure

* Indexed Lens/Traversal/Fold/etc.
    + updates that know where they are

* Tons of handy combinators

Using Prism
===========

The lens package provides a type "Prism" for sum types
< type Prism s t a b
< prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
< review :: Prism s t a b -> b -> t

which degrades into a Traversal

< Prism s t a b ~~> Traversal s t a b

Prism examples
==============

< _Left :: Prism (Either a c) (Either b c) a b
< _Left = prism Left $ \x -> case x of
<            Left  a -> Right a
<            Right r -> Left (Right r)

< review _Left           10 -- Left 10
< review (_Left . _Left) 10 -- Left (Left 10)

< toListOf (mapA . _Left) [Left 10, Right 'E', Left 20]
< -- [10,20]

< over (mapA . _Left) (*2) [Left 10, Right 'E', Left 20]
< -- [Left 20, Right 'E', Left 40]

Using Iso
=========

The lens package provides a type "Iso" which can have its direction "flipped"
< type Iso s t a b
< iso :: (s -> a) -> (b -> t) -> Iso s t a b
< from :: Iso s t a b -> Iso a b s t
< review :: Iso s t a b -> b -> t

and which degrades into a Traversal and a Prism

< Iso s t a b ~~> Traversal s t a b
< Iso s t a b ~~> Prism s t a b
