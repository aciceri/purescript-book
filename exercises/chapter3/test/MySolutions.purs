module Test.MySolutions where

import Prelude
import Data.List (head, filter, null, nubByEq)
import Data.Maybe (Maybe)
import Data.AddressBook (AddressBook, Entry)

-- Ex. 2
findEntryByStreet' :: String -> AddressBook -> Maybe Entry
findEntryByStreet' street = (filter filterEntry) >>> head
  where filterEntry entry = entry.address.street == street

-- Ex. 3
findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet street = (filter filterEntry) >>> head
  where filterEntry = _.address >>> _.street >>> eq street

-- Ex. 4
isInBook :: String -> String -> AddressBook -> Boolean
isInBook firstName lastName = filter filterEntry >>> null >>> not
  where filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

-- Ex. 5
removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubByEq sameEntries
  where sameEntries a b = a.firstName == b.firstName && a.lastName == b.lastName
