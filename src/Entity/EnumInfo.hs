module Entity.EnumInfo where

-- ( EnumInfo,
--   EnumValue,
--   new,
--   fromEnumInfo,
--   constBottom,
--   constTop,
--   constTopUnit,
--   constBool,
--   constBoolTrue,
--   constBoolFalse,
-- )

-- import qualified Context.Throw as Throw
-- import Control.Monad
-- import Data.Binary
-- import qualified Data.Set as S
-- import qualified Data.Text as T
-- import qualified Entity.BaseName as BN
-- import qualified Entity.DefiniteDescription as DD
-- import qualified Entity.Discriminant as D
-- import Entity.EnumTypeName
-- import qualified Entity.EnumValueName as EV
-- import qualified Entity.Hint as Hint
-- import qualified Entity.LocalLocator as LL
-- import qualified Entity.SourceLocator as SL
-- import qualified Entity.StrictGlobalLocator as SGL
-- import GHC.Generics

-- type EnumValue = (EV.EnumValueName, D.Discriminant) -- e.g. (this.core::top.unit, 0), (foo.bar.buz::color.yellow, 2)

-- newtype EnumInfo = EnumInfoCons {fromEnumInfo :: (EnumTypeName, [EnumValue])}
--   deriving (Generic, Show)

-- instance Binary EnumInfo

-- new :: Throw.Context m => Hint.Hint -> EnumTypeName -> [(T.Text, Maybe D.Discriminant)] -> m EnumInfo
-- new m enumTypeName itemList = do
--   itemList' <- mapM (attachPrefix m enumTypeName) $ setDiscriminant D.zero itemList
--   unless (isLinear (map snd itemList')) $
--     Throw.raiseError m "found a collision of discriminant"
--   return $ EnumInfoCons {fromEnumInfo = (enumTypeName, itemList')}

-- attachPrefix :: Throw.Context m => Hint.Hint -> EnumTypeName -> (T.Text, a) -> m (EV.EnumValueName, a)
-- attachPrefix m enumTypeName (enumValueName, d) = do
--   enumValue <- EV.new m enumTypeName enumValueName
--   return (enumValue, d)

-- setDiscriminant :: D.Discriminant -> [(a, Maybe D.Discriminant)] -> [(a, D.Discriminant)]
-- setDiscriminant discriminant clauseList =
--   case clauseList of
--     [] ->
--       []
--     (item, Nothing) : rest ->
--       (item, discriminant) : setDiscriminant (D.increment discriminant) rest
--     (item, Just value) : rest ->
--       (item, value) : setDiscriminant (D.increment value) rest

-- {-# INLINE isLinear #-}
-- isLinear :: [D.Discriminant] -> Bool
-- isLinear =
--   isLinear' S.empty

-- isLinear' :: S.Set D.Discriminant -> [D.Discriminant] -> Bool
-- isLinear' found input =
--   case input of
--     [] ->
--       True
--     (x : xs)
--       | x `S.member` found ->
--           False
--       | otherwise ->
--           isLinear' (S.insert x found) xs

-- constBottom :: EnumTypeName
-- constBottom =
--   EnumTypeName $ DD.newByGlobalLocator (SGL.baseGlobalLocatorOf SL.bottomLocator) [] BN.bottom

-- constTop :: EnumTypeName
-- constTop =
--   EnumTypeName $ DD.newByGlobalLocator (SGL.baseGlobalLocatorOf SL.topLocator) [] BN.top

-- constTopUnit :: EV.EnumValueName
-- constTopUnit =
--   EV.new' constTop $ LL.new [] BN.topUnit

-- {-# INLINE constBool #-}
-- constBool :: EnumTypeName
-- constBool =
--   EnumTypeName $ DD.newByGlobalLocator (SGL.baseGlobalLocatorOf SL.boolLocator) [] BN.bool

-- {-# INLINE constBoolTrue #-}
-- constBoolTrue :: EV.EnumValueName
-- constBoolTrue =
--   EV.new' constBool $ LL.new [] BN.boolTrue

-- {-# INLINE constBoolFalse #-}
-- constBoolFalse :: EV.EnumValueName
-- constBoolFalse =
--   EV.new' constBool $ LL.new [] BN.boolFalse
