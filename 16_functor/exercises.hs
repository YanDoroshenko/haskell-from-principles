{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Exercises where

data Quant a b = Finance | Desk a | Bloor b

instance Functor (Quant a) where
  fmap f (Bloor b) = Bloor $ f b
  fmap _ (Desk a) = Desk a
  fmap _ Finance = Finance

data K a b = K a

instance Functor (K a) where
  fmap _ (K a) = K a

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

newtype N a b = N a

instance Functor (Flip N a) where
  fmap f (Flip (N b)) = Flip $ N $ f b

data EvilGoateeConst a b = GoatyConst b

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst $ f b

data LiftItOut f a = LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where
  fmap g (LiftItOut f) = LiftItOut $ g <$> f

data Parappa f g a = DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap h (DaWrappa f g) = DaWrappa (h <$> f) (h <$> g)

data IgnoreOne f g a b = IgnoringSomething (f a) (g b)

instance Functor g => Functor (IgnoreOne f g a) where
  fmap h (IgnoringSomething f g) = IgnoringSomething f (h <$> g)

data Notorious g o a t = Notorious (g o) (g a) (g t)

instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious go ga gt) = Notorious go ga (f <$> gt)

data List a = Nil | Cons a (List a)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons h t) = Cons (f h) (f <$> t)

data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat $ f a
  fmap f (MoreGoats g1 g2 g3) = MoreGoats (f <$> g1) (f <$> g2) (f <$> g3)

data TalkToMe a = Halt | Print String a | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print s a) = Print s (f a)
  fmap g (Read f) = Read $ g <$> f
