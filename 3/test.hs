{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

type Name = String
type Age = Int
type Address = String
type PhoneNumber = Integer

newtype NameAgeTable = NAgT [(Name, Age)] deriving Show
newtype NameAddressTable = NAdT [(Name, Address)] deriving Show
newtype NamePhoneTable = NPT [(Name, PhoneNumber)] deriving Show

class SearchableByName t a | t -> a where
  search :: Name -> t -> Maybe a

instance SearchableByName NameAgeTable Age where
  search name (NAgT l) = lookup name l

instance SearchableByName NameAddressTable Address where
  search name (NAdT l) = lookup name l

instance SearchableByName NamePhoneTable PhoneNumber where
  search name (NPT l) = lookup name l

nameAge = NAgT [("Ana", 24), ("Gabriela", 21), ("Mihai", 25), ("Radu", 24)]
nameAddress = NAdT [("Mihai", "a random address"), ("Ion", "another address")]
namePhone = NPT [("Ana", 2472788), ("Mihai", 24828542)]
