-- file: ch04/Partial

isInAny needle haystack = any inSequence haystack
    where inSequence s = needl `isInfixOf` s

isInAny2 needle haystack = any (\s -> needle `isInfixOf` s) haystack

