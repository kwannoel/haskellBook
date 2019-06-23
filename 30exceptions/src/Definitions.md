class (Typeable e, Show e) =>
      Exception e where
  toException :: e -> SomeException
  fromException :: SomeException -> Maybe e
  displayException :: e -> String

SomeException acts as a parent type for all other exception types

data SomeException where
  SomeException
    :: Exception e => e -> SomeException
    
alternatively:
data SomeException =
  forall e . Exception e => SomeException e

Note here the type constructor is not taking an argument

the forall notation denotes for e of Exception class, there exists some e

any type that implements the Exception class can be that e and subsumed under the SomeException type
