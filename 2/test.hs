type Name = String
type Age = Int
type Address = String
type PhoneNumber = Integer

newtype NameAgeTable = NAgT [(Name, Age)] deriving Show
newtype NameAddressTable = NAdT [(Name, Address)] deriving Show
newtype NamePhoneTable = NPT [(Name, PhoneNumber)] deriving Show

nameAge = NAgT [("Ana", 24), ("Gabriela", 21), ("Mihai", 25), ("Radu", 24)]
nameAddress = NAdT [("Mihai", "a random address"), ("Ion", "another address")]
namePhone = NPT [("Ana", 2472788), ("Mihai", 24828542)]

searchNameAge name (NAgT l) = lookup name l
searchNameAddress name (NAdT l) = lookup name l
searchNamePhone name (NPT l) = lookup name l
