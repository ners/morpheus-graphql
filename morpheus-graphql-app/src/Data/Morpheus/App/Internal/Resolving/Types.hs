{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NamedFieldPuns #-}

module Data.Morpheus.App.Internal.Resolving.Types
  ( ResolverMap,
    NamedResolver (..),
    hoistNamedResolver,
    NamedResolverResult (..),
    NamedResolverRef (..),
    ResolverValue (..),
    hoistResolverValue,
    hoistLazyResolverValue,
    ObjectTypeResolver (..),
    hoistObjectTypeResolver,
    ResolverEntry,
    mkEnum,
    mkBoolean,
    mkFloat,
    mkInt,
    mkList,
    mkNull,
    mkString,
    mkObject,
    mkObjectMaybe,
    mkUnion,
    NamedResolverFun,
  )
where

import Control.Monad.Except (MonadError (throwError))
import qualified Data.HashMap.Lazy as HM
import Data.Morpheus.Internal.Ext (Merge (..))
import Data.Morpheus.Internal.Utils (IsMap (toAssoc), KeyOf (keyOf))
import Data.Morpheus.Types.Internal.AST
  ( FieldName,
    GQLError,
    ScalarValue (..),
    TypeName,
    ValidValue,
    internal,
  )
import GHC.Show (Show (show))
import Relude hiding (show)

type ResolverMap (m :: Type -> Type) = HashMap TypeName (NamedResolver m)

type NamedResolverArg = [ValidValue]

type NamedResolverFun m = NamedResolverArg -> m [NamedResolverResult m]

data NamedResolver (m :: Type -> Type) = NamedResolver
  { resolverName :: TypeName,
    resolverFun :: NamedResolverFun m
  }

instance Show (NamedResolver m) where
  show NamedResolver {..} =
    "NamedResolver { name = " <> show resolverName <> " }"

hoistNamedResolver :: (Functor m) => (forall a. m a -> n a) -> NamedResolver m -> NamedResolver n
hoistNamedResolver morphism NamedResolver {resolverName, resolverFun} =
  NamedResolver
    { resolverName
    , resolverFun = morphism . (fmap . fmap $ hoistNamedResolverResult morphism) . resolverFun
    }

newtype ObjectTypeResolver m = ObjectTypeResolver
  { objectFields :: HashMap FieldName (m (ResolverValue m))
  }

instance Show (ObjectTypeResolver m) where
  show ObjectTypeResolver {..} = "ObjectTypeResolver { " <> intercalate "," (map showField (toAssoc objectFields)) <> " }"
    where
      showField (name, _) = show name <> " = " <> "ResolverValue m"

hoistObjectTypeResolver :: (Functor m) => (forall a. m a -> n a) -> ObjectTypeResolver m -> ObjectTypeResolver n
hoistObjectTypeResolver morphism (ObjectTypeResolver {objectFields}) = ObjectTypeResolver {objectFields = hoistLazyResolverValue morphism <$> objectFields}

data NamedResolverRef = NamedResolverRef
  { resolverTypeName :: TypeName,
    resolverArgument :: NamedResolverArg
  }
  deriving (Show)

data NamedResolverResult (m :: Type -> Type)
  = NamedObjectResolver (ObjectTypeResolver m)
  | NamedUnionResolver NamedResolverRef
  | NamedEnumResolver TypeName
  | NamedScalarResolver ScalarValue
  | NamedNullResolver

hoistNamedResolverResult :: (Functor m) => (forall a. m a -> n a) -> NamedResolverResult m -> NamedResolverResult n
hoistNamedResolverResult morphism (NamedObjectResolver o) = NamedObjectResolver $ hoistObjectTypeResolver morphism o
hoistNamedResolverResult _ (NamedUnionResolver r) = NamedUnionResolver r
hoistNamedResolverResult _ (NamedEnumResolver n) = NamedEnumResolver n
hoistNamedResolverResult _ (NamedScalarResolver v) = NamedScalarResolver v
hoistNamedResolverResult _ NamedNullResolver = NamedNullResolver

instance KeyOf TypeName (NamedResolver m) where
  keyOf = resolverName

instance Show (NamedResolverResult m) where
  show NamedObjectResolver {} = "NamedObjectResolver"
  show NamedUnionResolver {} = "NamedUnionResolver"
  show NamedEnumResolver {} = "NamedEnumResolver"
  show NamedNullResolver {} = "NamedNullResolver"
  show NamedScalarResolver {} = "NamedScalarResolver"

data ResolverValue (m :: Type -> Type)
  = ResNull
  | ResScalar ScalarValue
  | ResList [ResolverValue m]
  | ResEnum TypeName
  | ResObject (Maybe TypeName) (ObjectTypeResolver m)
  | ResRef (m NamedResolverRef)
  | ResLazy (m (ResolverValue m))

hoistResolverValue :: (Functor m) => (forall a. m a -> n a) -> ResolverValue m -> ResolverValue n
hoistResolverValue _ ResNull = ResNull
hoistResolverValue _ (ResScalar v) = ResScalar v
hoistResolverValue morphism (ResList vs) = ResList $ hoistResolverValue morphism <$> vs
hoistResolverValue _ (ResEnum n) = ResEnum n
hoistResolverValue morphism (ResObject n o) = ResObject n $ hoistObjectTypeResolver morphism o
hoistResolverValue morphism (ResRef x) = ResRef $ morphism x
hoistResolverValue morphism (ResLazy l) = ResLazy $ hoistLazyResolverValue morphism l

hoistLazyResolverValue :: (Functor m) => (forall a. m a -> n a) -> m (ResolverValue m) -> n (ResolverValue n)
hoistLazyResolverValue morphism = morphism . fmap (hoistResolverValue morphism)

instance
  ( Monad m,
    Applicative f,
    MonadError GQLError m
  ) =>
  Merge f (ObjectTypeResolver m)
  where
  merge (ObjectTypeResolver x) (ObjectTypeResolver y) =
    pure $ ObjectTypeResolver (HM.unionWith mergeFields x y)
    where
      mergeFields a b = (,) <$> a <*> b >>= uncurry merge

instance Show (ResolverValue m) where
  show ResNull = "ResNull"
  show (ResScalar x) = "ResScalar:" <> show x
  show (ResList xs) = "ResList:" <> show xs
  show (ResEnum name) = "ResEnum:" <> show name
  show (ResObject name _) = "ResObject:" <> show name
  show ResRef {} = "ResRef {}"
  show ResLazy {} = "ResLazy {}"

instance IsString (ResolverValue m) where
  fromString = ResScalar . fromString

instance
  ( Monad f,
    MonadError GQLError f,
    Merge f (ObjectTypeResolver m)
  ) =>
  Merge f (ResolverValue m)
  where
  merge ResNull ResNull = pure ResNull
  merge ResScalar {} x@ResScalar {} = pure x
  merge ResEnum {} x@ResEnum {} = pure x
  merge (ResObject n x) (ResObject _ y) = ResObject n <$> merge x y
  merge _ _ = throwError (internal "can't merge: incompatible resolvers")

type ResolverEntry m = (FieldName, m (ResolverValue m))

--
mkString :: Text -> ResolverValue m
mkString = ResScalar . String

mkFloat :: Double -> ResolverValue m
mkFloat = ResScalar . Float

mkInt :: Int -> ResolverValue m
mkInt = ResScalar . Int

mkBoolean :: Bool -> ResolverValue m
mkBoolean = ResScalar . Boolean

mkList :: [ResolverValue m] -> ResolverValue m
mkList = ResList

mkNull :: ResolverValue m
mkNull = ResNull

mkEnum :: TypeName -> ResolverValue m
mkEnum = ResEnum

mkObject ::
  TypeName ->
  [ResolverEntry m] ->
  ResolverValue m
mkObject name = mkObjectMaybe (Just name)

mkObjectMaybe ::
  Maybe TypeName ->
  [ResolverEntry m] ->
  ResolverValue m
mkObjectMaybe name = ResObject name . ObjectTypeResolver . HM.fromList

mkUnion ::
  (Monad m) =>
  TypeName ->
  [ResolverEntry m] ->
  ResolverValue m
mkUnion name fields =
  ResObject
    (Just name)
    ObjectTypeResolver {objectFields = HM.fromList fields}
