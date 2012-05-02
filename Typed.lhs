The following exercise consists in write a function that prints the type of
a given monomorphic function. No, we are not talking about type-inference.
We will use the one that Haskell brings. Anyway, the task is to define a function `writeType`
that, given some Haskell value (which can be a function), print its type on the screen.

It's not a hard exercise, so it can be tried by a beginner that already knows how to work
with Haskell types. *Try it before read the solution*.

This is also my first attempt to create a blog post with **Pancod** and **HsColour**
from a literate Haskell source code.

Solution
--------

The approach of the solution is similar (identical) to the one taken in the `Data.Typeable` base module.
We will import the `intersperse` function, which will be useful when defining a pretty-printer for types.

> import Data.List (intersperse)

Our first mission is a function that, for some type `a`, returns its type signature.
Something like `typeOf :: a -> Type`. But we need to first define the `Type` datatype.

> data Type =
>    TCons String [Type]
>  | TList Type
>  | TTuple [Type]
>  | TFun Type Type
>    deriving Show

* The `TCons` constructor is for type constructors, like `Int`, `Maybe`, `IO` or `Map`.
* The `TList` for lists of a given type.
* The `TTuple` for tuples. With the empty list you get the *unit* type (`()`).
* Finally, `TFun` is the type constructor for functions.

Note that one can, actually, make all types only with the `TCons` constructor (think how if you
don't know it), but I still prefer this way.

Since we want of `typeOf` to run over every type, a good way to achieve this is to use a typeclass
and implement specific methods for each type.

> class Typed a where
>  typeOf :: a -> Type

Now is trivial to make some instances.

> instance Typed Int where
>  typeOf _ = TCons "Int" []
> 
> instance Typed Float where
>  typeOf _ = TCons "Float" []
> 
> instance Typed Bool where
>  typeOf _ = TCons "Bool" []
> 
> instance Typed Char where
>  typeOf _ = TCons "Char" []

And that's the way for types of null arity. Note that we always discard arguments.
This is necessary, because we really **need** to avoid depending in values.
We will have problems if the compiler tries to reduce some expression.
Even it would be nonsensical, because the type of a value has nothing to do
with one of its values.

Now, let's define our first type trick. If we want to define instances
for types with positive arity, we will need to apply `typeOf` with
argument(s) of the *inner(s)* type(s).

Here is provided the *deconstructor* for types with arity one.

> decons :: t a -> a
> decons = undefined

As you can see, it is not actually defined. All we need is to use its type, so
the definition does not matter. Note why we did not want of `typeOf` to try to
evaluate its argument.

Let's apply this to the `Maybe` type.

> instance Typed a => Typed (Maybe a) where
>  typeOf m = let t = typeOf $ decons m
>             in  TCons "Maybe" [t]

The same trick works with lists.

> instance Typed a => Typed [a] where
>  typeOf xs = let t = typeOf $ decons xs
>              in  TList t

It's the turn for tuples. We will do only the *2-uple*, since for other tuple orders
the same idea is valid. Since the constructor of *2-uples* has arity two, we need another
deconstructor.

> decons2 :: t a b -> (a,b)
> decons2 = undefined

I'm sure you already figure out how to define the `deconsN` function for any `N`.
Using the deconstructor with tuples we have the following instance.

> instance (Typed a,Typed b) => Typed (a,b) where
>  typeOf tup = let (x,y) = decons2 tup
>               in  TTuple [typeOf x,typeOf y]

The good thing is that all types are traversed recursively. For example,
with `typeOf (1,Just 2)`, it's reduced to `TTuple [typeOf 1,typeOf (Just 2)]`,
then to `TTuple [TCons "Int" [], TCons "Maybe" [typeOf 2]]`, and finally to
`TTuple [TCons "Int" [], TCons "Maybe" [TCons "Int" []]]`. Well, this evaluation
is not true, but it works like that (replacing with `undefined`s everywhere!).
What does this work is the Haskell type system. We are only playing
with types, never with values.

The last instance we will do is for the function type constructor. Though, if
you think about it, there is not something new. The arrow `->` is just a
type constructor with arity 2.

> instance (Typed a,Typed b) => Typed (a -> b) where
>  typeOf f = let (x,y) = decons2 f
>             in  TFun (typeOf x) (typeOf y)

However, our problem does not end here (though here ends the most interesting part).
The problem was to **print** the type of a given function. The next step is to write
a pretty-printer function for types.

First, it will be handy to have a function that tell us if a type will need to be parenthesized when
appears as an argument for some type constructor. For example, `Int -> Int` in `Maybe (Int -> Int)`.

> plural :: Type -> Bool
> plural (TCons _ xs) = not $ null xs
> plural (TFun _ _) = True
> plural _ = False

An argument of an applied type constructor only will need to be parenthesized when its arity is not null.
A function always will need it (because it's a constructor with arity two). No other will
thanks to the syntax of tuples and list types. They are already parenthesized in some way.

However if the type constructor is the arrow `->` the parenthesis are only needed when the **left**
argument is a function type, since is infix and *right-associative*. For example, `Maybe Float -> (Float -> Float)`
does not need parenthesis (I put them to make clear the association order), but `(Maybe Float -> Float) -> Float` needs them.
Let's define then a function that test if a type is functional.

> isFun :: Type -> Bool
> isFun (TFun _ _) = True
> isFun _ = False

To surround an expression with parenthesis we define the `par` function.

> par :: String -> String
> par str = concat ["(",str,")"]

It's time for our `printType :: Type -> String` function. For expressions that must be
parenthesized when needed we will use the variant `printTypeIf`. It will put parenthesis
when a test function holds.

> printTypeIf :: (Type -> Bool) -> Type -> String
> printTypeIf f t = (if f t then par else id) $ printType t

Now the full pretty-printer, using all the mentioned above.

> printType :: Type -> String
> printType (TCons n ts) = unwords $ n : fmap (printTypeIf plural) ts
> printType (TList t) = concat [ "[" , printType t , "]" ]
> printType (TTuple ts) = par . concat $ intersperse ", " $ fmap printType ts
> printType (TFun t1 t2) = unwords [ printTypeIf isFun t1 , "->" , printType t2 ]

Finally, the required function `writeType :: Typed a => a -> IO ()` can be written now
immediately.

> writeType :: Typed a => a -> IO ()
> writeType = putStrLn . printType . typeOf

So we are done! You can try the next example:

> example :: (Int -> Int) -> Maybe Bool -> Maybe (Int -> Int)
> example f mb = fmap (\b -> if b then const 0 else f) mb

*And that's all!*

Closure
-------

I think this is a very funny exercise, and that's why I posted it here.
I hope you enjoy it like I did. You can get the code of this post from
[GitHub](https://github.com/Daniel-Diaz/literates/blob/master/Typed.lhs).

Good luck, Daniel Díaz.