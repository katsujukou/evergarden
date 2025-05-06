module Evergarden.Schema.Routing.RouteParams where

import Prelude

import Data.Array as Array
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), Product(..), Sum(..), from)
import Data.Maybe (Maybe(..))
import Unsafe.Coerce (unsafeCoerce)

-- | An opaque type whose inhabits are arbitrary values given as route parameters.
data AnyParam

unsafeToAnyParam :: forall a. a -> AnyParam
unsafeToAnyParam = unsafeCoerce

unsafeFromAnyParam :: forall a. AnyParam -> a
unsafeFromAnyParam = unsafeCoerce

routeParamsTest :: forall a. RouteParams a => a -> Array AnyParam
routeParamsTest a = routeParams a

unsafeApplyParam :: forall a b. a -> AnyParam -> b
unsafeApplyParam f p = (unsafeCoerce f) p

unsafeApplyParam1 :: forall a b. a -> AnyParam -> b
unsafeApplyParam1 f a = (unsafeCoerce f) a

unsafeApplyParam2 :: forall a b. a -> AnyParam -> AnyParam -> b
unsafeApplyParam2 f a b = (unsafeCoerce f) a b

unsafeApplyParam3 :: forall a b. a -> AnyParam -> AnyParam -> AnyParam -> b
unsafeApplyParam3 f a b c = (unsafeCoerce f) a b c

unsafeApplyParam4 :: forall a b. a -> AnyParam -> AnyParam -> AnyParam -> AnyParam -> b
unsafeApplyParam4 f a b c d = (unsafeCoerce f) a b c d

unsafeApplyParam5 :: forall a b. a -> AnyParam -> AnyParam -> AnyParam -> AnyParam -> AnyParam -> b
unsafeApplyParam5 f a b c d e = (unsafeCoerce f) a b c d e

unsafeApplyParam6
  :: forall a b
   . a
  -> AnyParam
  -> AnyParam
  -> AnyParam
  -> AnyParam
  -> AnyParam
  -> AnyParam
  -> b
unsafeApplyParam6 f a b c d e f' = (unsafeCoerce f) a b c d e f'

unsafeApplyParam7
  :: forall a b
   . a
  -> AnyParam
  -> AnyParam
  -> AnyParam
  -> AnyParam
  -> AnyParam
  -> AnyParam
  -> AnyParam
  -> b
unsafeApplyParam7 f a b c d e f' g = (unsafeCoerce f) a b c d e f' g

unsafeApplyParam8
  :: forall a b
   . a
  -> AnyParam
  -> AnyParam
  -> AnyParam
  -> AnyParam
  -> AnyParam
  -> AnyParam
  -> AnyParam
  -> AnyParam
  -> b
unsafeApplyParam8 f a b c d e f' g h = (unsafeCoerce f) a b c d e f' g h 

unsafeApplyParam9
  :: forall a b
   . a
  -> AnyParam
  -> AnyParam
  -> AnyParam
  -> AnyParam
  -> AnyParam
  -> AnyParam
  -> AnyParam
  -> AnyParam
  -> AnyParam
  -> b
unsafeApplyParam9 f a b c d e f' g h i  = (unsafeCoerce f) a b c d e f' g h i

unsafeApplyParams :: forall a b. a -> Array AnyParam -> b
unsafeApplyParams fn = case _ of
  [] -> unsafeCoerce fn
  [ a ] -> unsafeApplyParam1 fn a
  [ a, b ] -> unsafeApplyParam2 fn a b
  [ a, b, c ] -> unsafeApplyParam3 fn a b c
  [ a, b, c, d ] -> unsafeApplyParam4 fn a b c d
  [ a, b, c, d, e ] -> unsafeApplyParam5 fn a b c d e
  [ a, b, c, d, e, f ] -> unsafeApplyParam6 fn a b c d e f
  [ a, b, c, d, e, f, g ] -> unsafeApplyParam7 fn a b c d e f g
  [ a, b, c, d, e, f, g, h ] -> unsafeApplyParam8 fn a b c d e f g h
  [ a, b, c, d, e, f, g, h, i ] -> unsafeApplyParam9 fn a b c d e f g h i
  xs -> unsafeApplyParamN fn xs

unsafeApplyParamN :: forall a b. a -> Array AnyParam -> b
unsafeApplyParamN f xs = go f xs
  where
  go acc = Array.uncons >>> case _ of
    Nothing -> unsafeCoerce acc
    Just { head, tail } -> go (unsafeApplyParam acc head) tail

class RouteParams a where
  routeParams :: a -> Array AnyParam

instance routeParamsRep :: 
  ( Generic a rep
  , RouteRepParams rep
  ) =>
  RouteParams a
  where
  routeParams a = routeRepParams (from a)

class RouteRepParams rep where
  routeRepParams :: rep -> Array AnyParam

instance routeRepParamsCtr ::
  ( RouteConstructorArgParams args
  ) =>
  RouteRepParams (Constructor ctr args)
  where
  routeRepParams (Constructor arg) = routeConstructorArgParams arg

instance routeRepParamsSum ::
  ( RouteRepParams l
  , RouteRepParams r
  ) =>
  RouteRepParams (Sum l r)
  where
  routeRepParams = case _ of
    Inl l -> routeRepParams l
    Inr r -> routeRepParams r

class RouteConstructorArgParams args where
  routeConstructorArgParams :: args -> Array AnyParam

instance routeCtrArgParamsArg ::
  RouteConstructorArgParams (Argument t)
  where
  routeConstructorArgParams (Argument a) = [ unsafeToAnyParam a ]

else instance routeCtrArgParamsProduct ::
  ( RouteConstructorArgParams r
  ) =>
  RouteConstructorArgParams (Product (Argument l) r)
  where
  routeConstructorArgParams (Product l r)
    | Argument a <- l =
        Array.cons
          (unsafeToAnyParam a)
          (routeConstructorArgParams r)

else instance routeCtrArgParamsAny ::
   RouteConstructorArgParams other
  where
  routeConstructorArgParams _ = []