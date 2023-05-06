compareLastNames :: (String, String) -> (String, String) -> Ordering
compareLastNames (fn1, ln1) (fn2, ln2) =
  let cmp1 = compare ln1 ln2
   in let cmp2 = compare fn1 fn2
       in if cmp1 == EQ
            then cmp2
            else cmp1
