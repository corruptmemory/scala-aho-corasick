# A reasonably efficient implementation of Aho-Corasick in Scala

This is an imperative implementation of the [Aho-Corasick](http://en.wikipedia.org/wiki/Aho%E2%80%93Corasick_string_matching_algorithm) string-matching
algorithm written entirely in Scala.  It is reasonably efficient, and is character-oriented rather
than byte-oriented.

## Usage

It is extremely easy to use the library since basically all it does is find matching strings in an
input document.  There are two ways to use the builder, either using the factory constructor that
takes in a `Seq` example:

    import com.corruptmemory.aho_corasick.AhoCorasickBuilder

    val builder = AhoCorasickBuilder[Unit](List(("he",()),("she",()),("his",()),("hers",()),("her",())))
    val finder = builder.build()

    val results = finder.find("Several ushers rushed over to aid her in finding a seat.")
    // => Vector(Match(10,he,he,()), Match(9,she,she,()), Match(10,her,her,()), Match(10,hers,hers,()), Match(18,he,he,()), Match(17,she,she,()), Match(34,he,he,()), Match(34,her,her,()))

You can also use the `+=` operator on the builder to add elements

    builder += "it" -> ()

### build()

When the `build()` method is invoked returns a finder and clears out all the data in the builder.
It is possible to reuse the builder without interfering with already generated finders.

## Matched results

Results are returned in a `Match` value that is defined as follows:

    case class Match[T](start:Int,target:String,actual:String,data:T)

The `start` value is the offset in characters from the beginning of the input string to the first
letter of the match.  `target` is the string to match, `actual` was the actual string matched.  It
is possible that `target` and `actual` can differ (for example in case) because one of the optional
arguments to the builder is a character map function that gets applied to each character in the
dictionary (trie) and to each character during find.  The default character map function maps
everything to lower-case therefore case-insensitive matching is the default.  Other interesting
character map functions could include removing diacritical or accent marks from characters. `data`
is arbitrary data associated with the dictionary entries that matched.  You can supply the data when
you add a word to the dictionary:

    builder += "word" -> <data>

The data added must conform to a type, in the above example the type was `Unit` so `()` was supplied
as the value.

## License

This library is released under the Apache 2.0 license.