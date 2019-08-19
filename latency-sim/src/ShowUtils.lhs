```{.haskell .hidden}
module ShowUtils where
```

Joins a list of `ShowS` functions with separators.
```{.haskell .literate}
joinWith :: ShowS -> [ShowS] -> ShowS
joinWith _   []   = id
joinWith sep args = joins sep `foldr1` args

joins :: ShowS -> ShowS -> ShowS -> ShowS
joins sep a b  = a . sep . b
```

Join `ShowS` functions with new line separators.
```{.haskell .literate}
joinLines :: [ShowS] -> ShowS
joinLines = joinWith ("\n"++)
```

