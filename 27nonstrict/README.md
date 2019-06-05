# 27nonstrict

strict: inside out, arguments are evaluated first

non-strict: 
- outside in, function evaluates arguments when needed
- able to have bottoms as long are bottoms are never forced

true laziness:
- memorizes results of **all** evaluated functions: uses large amts of memory

thunk: created for each expression, sort of like a placeholder for values, if the values were never needed, it gets swept away by the garbage collector

evaluation:

- call-by-name

- call-by-need

sharing in context of non-strict code
- To prevent sharing: inline expressions e.g. rather than `f = 1` we do `f _ = 1`.

methods for observing sharing behavior and program efficiency
- :sprint

stuff which shares:
- once applied
- pointfree
- let, in blocks

stuff which doesnt share:
- typeclass vars
- in core they are functions awaiting a concretising argument
- lambdas

refutable & irrefutable patterns
potential fail to match vs never fail to match
`f True = False vs f _ = True` 

irrefutable:
lazyPattern :: (a, b) -> String
lazyPattern ~(a, b) = const "Cousin It" a

cant discriminate cases of a sum, useful for unpacking products which might not get used

bottoming out

examining ghc core:
- :set -ddump-simpl
- :l code/sample.hs
- :set -dsuppress-all

to make arguments strict:
- use bang / seq
- both convert to case match under the hood in ghc core

we can use (!) also in data type

e.g. data Foo = Foo Int !Int

usecases would be when it's small e.g. Int & Double values

rule of the thumb : lazy in the spine, strict in the leaves

if you have lots of Int and Double all thunked, they would consume lots of mem

another tool in our toolbox is {-# LANGUAGE Strict #-}

e.g. blah x = 1 converts to blah x = x `seq` 1 or blah !x = 1

to escape fringe cases we can then use tilde

note that ! and seq only evaluate to WHNF, use them on datatypes itself to 100% strict

note for evaluating variables:
- > let x = id 1
- > :sprint x 
- `x = _` (x has not been evaluated)
- > x
- `1`
- > :sprint x
- `1` (x has been evaluated to a constant var and is now shareable)

notice above x is a concrete type and hence can be shared, the same cannot be said for polymorphic variables e.g.
> let x :: Num a => a; x = 1
> x
> :sprint x
x = _
