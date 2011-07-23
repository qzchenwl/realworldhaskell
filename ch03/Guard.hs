-- file ch03/Guard.hs

fromMaybe defval wrapped =
    case wrapped of
        Nothing    -> defval
        Just value -> value

nodesAreSame (Node a _ _) (Node b _ _)
    | a == b     = Just a

nodesAreSame _ _ = Nothing
