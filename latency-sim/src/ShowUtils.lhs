```{.haskell .hidden}
module ShowUtils where
```

Joins a list of `ShowS` functions with separators.
```{.haskell .literate}
joinWith :: ShowS -> [ShowS] -> ShowS
joinWith sep = foldr1 $ joins sep

joins :: ShowS -> ShowS -> ShowS -> ShowS
joins sep a b  = a . sep . b
```

Join `ShowS` functions with new line separators.
```{.haskell .literate}
joinLines :: [ShowS] -> ShowS
joinLines = joinWith ("\n"++)
```

