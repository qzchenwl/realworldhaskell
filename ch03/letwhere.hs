-- file: ch03/letwhere.hs

bar = let b = 2
          c = True
      in let a = b
         in (a, c)

foo = x
      where x = y
              where y = 2
