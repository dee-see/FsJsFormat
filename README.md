# F# JS Formatter

This is a totally-incomplete JS formatter that doesn't handle a ton of edge cases and isn't tested at all. I built it for a personal project and will patch it as it fails.

On a 5.3 MB bundle used by a popular website, this tools ran 10x faster than `prettier`. The output wasn't the same but it was good enough for my current use-case (grepping through scraped JavaScript files)

Built using dotnetcore3.0.
