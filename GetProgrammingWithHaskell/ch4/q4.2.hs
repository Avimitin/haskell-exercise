addressLetter :: (String, String) -> String -> String
addressLetter name location = case location of
  "DC" -> dcOffice name
  _ -> uncurry (++) name
  where
    dcOffice (fstName, lstName) = "Esq." ++ fstName ++ " " ++ lstName ++ ": Washington, DC"
