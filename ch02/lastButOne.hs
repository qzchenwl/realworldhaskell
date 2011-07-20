-- file: ch02/lastButOne.hs
lastButOne xs = last (take (length xs - 1) xs)
