{-|
Module:      Data.Deriving
Copyright:   (C) 2015-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Portability: Template Haskell

This module reexports all of the functionality of the other modules in this library
(with the exception of "Data.Deriving.Via", which is only available on GHC 8.2 or
later). This module also provides a high-level tutorial on @deriving-compat@'s
naming conventions and best practices. Typeclass-specific information can be found
in their respective modules.
-}
module Data.Deriving (
      -- * Backported changes
      -- $changes

      -- * @derive@- functions
      -- $derive

      -- * @make@- functions
      -- $make
      module Exports
    ) where

import Data.Bounded.Deriving     as Exports
import Data.Enum.Deriving        as Exports
import Data.Eq.Deriving          as Exports
import Data.Foldable.Deriving    as Exports
import Data.Functor.Deriving     as Exports
import Data.Ix.Deriving          as Exports
import Data.Ord.Deriving         as Exports
import Data.Traversable.Deriving as Exports
import Text.Read.Deriving        as Exports
import Text.Show.Deriving        as Exports

{- $changes
The following changes have been backported:

* In GHC 7.2, deriving 'Read' was changed so that constructors that use
  @MagicHash@ now parse correctly.

* In GHC 7.8, deriving standalone 'Read' instances was fixed to avoid crashing on
  datatypes with no constructors. Derived 'Read' instances were also changed so
  as to compile more quickly.

* In GHC 7.10, deriving standalone 'Read' and 'Show' instances were fixed to ensure
  that they use the correct fixity information for a particular datatype.

* In GHC 8.0, @DeriveFoldable@ was changed to allow folding over data types with
  existential constraints.

* In GHC 8.0, @DeriveFoldable@ and @DeriveTraversable@ were changed so as not to
  generate superfluous 'mempty' or 'pure' expressions in generated code. As a result,
  this allows deriving 'Traversable' instances for datatypes with unlifted argument
  types.

* In GHC 8.0, deriving 'Ix' was changed to use @('&&')@ instead of @if@, as the latter
  interacts poorly with @RebindableSyntax@. A bug was also fixed so that
  standalone-derived 'Ix' instances for single-constructor GADTs do not crash GHC.

* In GHC 8.0, deriving 'Show' was changed so that constructor fields with unlifted
  types are no longer shown with parentheses, and the output of showing an unlifted
  type is suffixed with the same number of hash signs as the corresponding primitive
  literals.

* In GHC 8.2, deriving 'Ord' was changed so that it generates concrete @if@-expressions
  that are not subject to @RebindableSyntax@. It was also changed so that derived
  @('<=')@, @('>')@, and @('>=')@ methods are expressed through @('<')@, which avoids
  generating a substantial amount of code.

* In GHC 8.2, deriving 'Traversable' was changed so that it uses 'liftA2' to implement
  'traverse' whenever possible. This was done since 'liftA2' was also made a class
  method of 'Applicative', so sometimes using 'liftA2' produces more efficient code.

* In GHC 8.2, deriving 'Show' was changed so that it uses an explicit @showCommaSpace@
  method, instead of repeating the code @showString \", \"@ in several places.

* In GHC 8.2, @DeriveFunctor@ was changed so that it derives implementations of
  ('<$').

* In GHC 8.4, @DeriveFoldable@ was changed so that it derives implementations of
  'null'.

* In GHC 8.4, deriving 'Functor' and 'Traverable' was changed so that it uses 'coerce'
  for efficiency when the last parameter of the data type is at phantom role.

* In GHC 8.4, the @EmptyDataDeriving@ proposal brought forth a slew of changes related
  to how instances for empty data types (i.e., no constructors) were derived. These
  changes include:

    * For derived 'Eq' and 'Ord' instances for empty data types, simply return
      'True' and 'EQ', respectively, without inspecting the arguments.

    * For derived 'Read' instances for empty data types, simply return 'pfail'
      (without 'parens').

    * For derived 'Show' instances for empty data types, inspect the argument
      (instead of 'error'ing).

    * For derived 'Functor' and 'Traversable' instances for empty data
      types, make 'fmap' and 'traverse' strict in its argument.

    * For derived 'Foldable' instances, do not error on empty data types.
      Instead, simply return the folded state (for 'foldr') or 'mempty' (for
      'foldMap'), without inspecting the arguments.

* In GHC 8.6, the @DerivingVia@ language extension was introduced.
  @deriving-compat@ provides an interface which attempts to mimic this
  extension (as well as @GeneralizedNewtypeDeriving@, which is a special case
  of @DerivingVia@) as closely as possible.

  Since the generated code requires the use of @TypeApplications@, this can
  only be backported back to GHC 8.2.

* In GHC 8.6, deriving 'Read' was changed so as to factor out certain commonly
  used subexpressions, which significantly improve compliation times.

* In GHC 8.10, @DerivingVia@ permits \"floating\" type variables in @via@ types,
  such as the @a@ in @'deriveVia' [t| forall a. Show MyInt ``Via`` Const Int a |]@.

* In GHC 8.12, @DeriveFunctor@ was changed so that it works on more
  constructors with rank-n field types.
-}

{- $derive

Functions with the @derive@- prefix can be used to automatically generate an instance
of a typeclass for a given datatype 'Name'. Some examples:

@
&#123;-&#35; LANGUAGE TemplateHaskell &#35;-&#125;
import Data.Deriving

data Pair a = Pair a a
$('deriveFunctor' ''Pair) -- instance Functor Pair where ...

data Product f g a = Product (f a) (g a)
$('deriveFoldable' ''Product)
-- instance (Foldable f, Foldable g) => Foldable (Pair f g) where ...
@

If you are using @template-haskell-2.7.0.0@ or later (i.e., GHC 7.4 or later),
then @derive@-functions can be used with data family instances (which requires the
@-XTypeFamilies@ extension). To do so, pass the 'Name' of a data or newtype instance
constructor (NOT a data family name!) to @deriveFoldable@.  Note that the
generated code may require the @-XFlexibleInstances@ extension. Example:

@
&#123;-&#35; LANGUAGE FlexibleInstances, TemplateHaskell, TypeFamilies &#35;-&#125;
import Data.Deriving

class AssocClass a b where
    data AssocData a b
instance AssocClass Int b where
    data AssocData Int b = AssocDataInt1 Int
                         | AssocDataInt2 b
$('deriveFunctor' 'AssocDataInt1) -- instance Functor (AssocData Int) where ...
-- Alternatively, one could use $(deriveFunctor 'AssocDataInt2)
@

@derive@-functions in @deriving-compat@ fall into one of three categories:

* Category 0: Typeclasses with an argument of kind @*@.
  ('deriveBounded', 'deriveEnum', 'deriveEq', 'deriveIx', 'deriveOrd', 'deriveRead', 'deriveShow')

* Category 1: Typeclasses with an argument of kind @* -> *@, That is, a datatype
  with such an instance must have at least one type variable, and the last type
  variable must be of kind @*@.
  ('deriveEq1', 'deriveFoldable', 'deriveFunctor', 'deriveOrd1',
   'deriveRead1', 'deriveShow1', 'deriveTraversable')

* Category 2: Typeclasses with an argument of kind @* -> * -> *@. That is, a datatype
  with such an instance must have at least two type variables, and the last two type
  variables must be of kind @*@.
  ('deriveEq2', 'deriveOrd2', 'deriveRead2', 'deriveShow2')

Note that there are some limitations to @derive@-functions:

* The 'Name' argument must not be of a type synonym.

* Type variables (other than the last ones) are assumed to require typeclass
  constraints. The constraints are different depending on the category. For example,
  for Category 0 functions, other type variables of kind @*@ are assumed to be
  constrained by that typeclass. As an example:

  @
  data Foo a = Foo a
  $(deriveEq ''Foo)
  @

  will result in a generated instance of:

  @
  instance Eq a => Eq (Foo a) where ...
  @

  If you do not want this behavior, use a @make@- function instead.

* For Category 1 and 2 functions, if you are using the @-XDatatypeContexts@ extension,
  a constraint cannot mention the last type variables. For example,
  @data Illegal a where I :: Ord a => a -> Illegal a@ cannot have a derived 'Functor'
  instance.

* For Category 1 and 2 functions, if one of the last type variables is used within a
  constructor field's type, it must only be used in the last type arguments. For
  example, @data Legal a = Legal (Either Int a)@ can have a derived 'Functor' instance,
  but @data Illegal a = Illegal (Either a Int)@ cannot.

* For Category 1 and 2 functions, data family instances must be able to eta-reduce the
  last type variables. In other words, if you have a instance of the form:

  @
  data family Family a1 ... an t1 ... tn
  data instance Family e1 ... e2 v1 ... vn = ...
  @

  where @t1@, ..., @tn@ are the last type variables, then the following conditions
  must hold:

  1. @v1@, ..., @vn@ must be type variables.
  2. @v1@, ..., @vn@ must not be mentioned in any of @e1@, ..., @e2@.

-}

{- $make

Functions prefixed with @make@- are similar to @derive@-functions in that they also
generate code, but @make@-functions in particular generate the expression for a
particular typeclass method. For example:

@
&#123;-&#35; LANGUAGE TemplateHaskell &#35;-&#125;
import Data.Deriving

data Pair a = Pair a a

instance Functor Pair where
    fmap = $('makeFmap' ''Pair)
@

In this example, 'makeFmap' will splice in the appropriate lambda expression which
implements 'fmap' for @Pair@.

@make@-functions are subject to all the restrictions of @derive@-functions listed
above save for one exception: the datatype need not be an instance of a particular
typeclass. There are some scenarios where this might be preferred over using a
@derive@-function. For example, you might want to map over a @Pair@ value
without explicitly having to make it an instance of 'Functor'.

Another use case for @make@-functions is sophisticated data types—that is, an
expression for which a @derive@-function would infer the wrong instance context.
Consider the following example:

@
data Proxy a = Proxy
$('deriveEq' ''Proxy)
@

This would result in a generated instance of:

@
instance Eq a => Eq (Proxy a) where ...
@

This compiles, but is not what we want, since the @Eq a@ constraint is completely
unnecessary. Another scenario in which @derive@-functions fail is when you
have something like this:

@
newtype HigherKinded f a b = HigherKinded (f a b)
$('deriveFunctor' ''HigherKinded)
@

Ideally, this would produce @HigherKinded (f a)@ as its instance context, but sadly,
the Template Haskell type inference machinery used in @deriving-compat@ is not smart
enough to figure that out. Nevertheless, @make@-functions provide a valuable
backdoor for these sorts of scenarios:

@
&#123;-&#35; LANGUAGE FlexibleContexts, TemplateHaskell &#35;-&#125;
import Data.Foldable.Deriving

data Proxy a = Proxy
newtype HigherKinded f a b = HigherKinded (f a b)

instance Eq (Proxy a) where
    (==) = $('makeEq' ''Proxy)

instance Functor (f a) => Functor (HigherKinded f a) where
    fmap = $('makeFmap' ''HigherKinded)
@

-}
