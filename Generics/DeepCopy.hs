{-|
Module      : Generics.DeepCopy
Description : Generic deep copy for references in the IO monad
Copyright   : (c) Daniel Gröber <dxld ÄT darkboxed DOT org>
License     : BSD3
Stability   : experimental

This module provides the class 'DeepCopy' which can be used to make recursive
copies of arbitrary datatypes containing (possibly nested) references. Currently
'IORef's and 'MVar's are copied.

'DeepCopy' itself is only an abstraction for the two implementations of the
functionality this library provideds: 'DeepCopyGHC' and 'makeDeepCopy', using
"GHC.Generics" and TemplateHaskell respectively.

To use this module you must import @Generics.DeepCopy@ as well as __one__ of the
implementation modules:

* "Generics.DeepCopy.GHC" uses "GHC.Generics" which allows users to make
instances of 'DeepCopy' without having to specify an implementation, like so:

@
instance DeepCopyGHC SomeType
@

Instances of 'DeepCopyGHC' are automatically turned into 'DeepCopy' instances,
the distinction is only there so this library can provide a TemplateHaskell
based approach as well.

The default implementation of DeepCopyGHC works so long as SomeType is not a
bultin type (like 'Char', 'Int', etc.) and all other types used in SomeType
(including itself) have a 'Generic' instance (which can be effortlessly derived
using @-XDeriveGeneric@)

If SomeType is a builtin type then doing the following will work just fine since
a primitive type won't contain any references anyways.

@
instance DeepCopyGHC PrimType where deepCopyGHC x = return x
@


* "Generics.DeepCopy.TH" uses TemplateHaskell to allow the user to easily make
instances of 'DeepCopy' without the need for nested types to be instances of
'DeepCopyTH' themselves. For example:

@
data SomeComplexType = SomeComplexType SomeOtherType (IORef Int)
data SomeOtherType = ...

$(makeDeepCopy ''SomeComplexType) -- This expands to an instance declaration
                                  -- for 'DeepCopyTH' 'SomeComplexType'
@

Here @SomeOtherType@ does not need to be an instance of DeepCopy already as
'makeDeepCopy' takes care of all nested types itself.

It is still possible to make custom instances of 'DeepCopyTH' though and it will
work as expected:

@
data SomeComplexType = SomeComplexType SomeOtherType (IORef Int)
data SomeOtherType = SomeOtherType String String String

instance DeepCopyTH SomeOtherType where
    deepCopyTH x = putStrLn "hello SomeOtherType" >> return x

$(makeDeepCopy ''SomeComplexType) -- In this case the instance for
                                  -- SomeComplexType will use the instance for
                                  -- SomeOtherType instead of handling it itself
@


The TemplateHaskell way of making 'DeepCopy' instances is useful for libraries
as often one will need to make lots of orphan instances of 'Generic' as many
types don't have them yet. Doing this in is discouraged however as it can easily
lead to problems when the user of the library links with another library that
also defines an instance for 'Generic' for the same type.

-}
module Generics.DeepCopy where

class DeepCopy a where
    -- | Traverse the datatype @a@, replacing all references with a new
    -- reference containing a 'deepCopy' of the original value.
    deepCopy :: a -> IO a
