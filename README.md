# Dupe Recoup

Recover the items in a csv most likely to be duplicates of each other

Pass in a row of "Scorers", a function `Text -> Text -> Float` to allow different scores per column

This code was written for a demonstrative blog post, so is somewhat naive (it generates all pairs of rows then takes scores for each pair, without optimizations)
