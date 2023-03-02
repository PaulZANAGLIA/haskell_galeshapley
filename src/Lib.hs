module Lib
    ( galeShapley
    ) where

-- Define a type to represent preferences
type Preferences = [(Int, [Int])]

-- Define a data type to represent a person
data Person = Person {
  name :: Int,
  prefList :: [Int],
  partner :: Maybe Person
}

-- Define a function to create a list of persons from a preference list
createPersons :: Preferences -> [Person]
createPersons prefs = [Person i prefs' Nothing | (i, prefs') <- prefs]

-- Define a function to find a person by name
findPerson :: Int -> [Person] -> Person
findPerson name persons = head [p | p <- persons, name p == name]

-- Define a function to propose to a person
proposeTo :: Person -> Person -> [Person] -> [Person]
proposeTo proposer target persons
  | isJust (partner target) = target : persons -- Target already has a partner, reject proposal
  | otherwise = [updatedProposer, updatedTarget] ++ otherPersons -- Update partners and return updated list of persons
  where
    updatedProposer = proposer { partner = Just target }
    updatedTarget = target { partner = Just proposer }
    otherPersons = filter (\p -> name p /= name updatedProposer && name p /= name updatedTarget) persons

-- Define a function to run the Gale-Shapley algorithm
galeShapley :: Preferences -> [(Int, Int)]
galeShapley prefs = map (\(p1, p2) -> (name p1, name p2)) pairs
  where
    persons = createPersons prefs
    freePersons = persons -- Initially all persons are free
    pairs = galeShapley' freePersons []

    galeShapley' :: [Person] -> [(Person, Person)] -> [(Person, Person)]
    galeShapley' [] pairs = pairs -- All pairs have been formed, return the list of pairs
    galeShapley' (p : ps) pairs =
      let pref = prefList p
          target = findPerson (head pref) persons
          updatedPersons = proposeTo p target (p : ps)
          newPairs = if isJust (partner p) then (p, fromJust (partner p)) : pairs else pairs
      in galeShapley' updatedPersons newPairs

