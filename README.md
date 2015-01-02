Unified source code storage system
==========

![Tabs vs spaces](http://www.emacswiki.org/pics/static/TabsSpacesBoth.png)

When you have had your last argument: tabs vs spaces? If you hadn't, we envy you. The fact is - there are many coding standards and pretty much every developer prefers its own. The companies or large teams of developers usually agree on the common list of rules to follow. Needless to say, this way of doing things turns out not to be as effective as it seems. It leads to unnecessary git commits because you have to make corrections after "that guy" -- and after a year of two, the source code is a mess anyway.

How to solve this problem? Much has been written on this subject to date. There exist code reformatters, usually part of expensive IDEs, making you unable to integrate them into your workflow.

This project allows the source code to always be stored in some predefined and/or compact meta format, which can be enforced via a git hook. However, when a programmer opens that file, it is pretty-printed according to his specification. The same, just inverted, process occurs when one saves / commits the file; the written source code is parsed back to that meta format.

This project is based on InvertibleSyntax, described in a paper by Tillmann Rendel and Klaus Ostermann published in 2010.

Batteries included
---

### JSON

As a proof of concept, the JSON parser and pretty printer is included.

We wanted to have the abstract syntax as accurate and usable as possible. Therefore:

* JSON object is parsed to `Map String JValue` for efficient key lookup.
* JSON string with escape sequences is unescaped, so you get the actual string you can use right away.

The invertible parser library enables us to parse escape sequences to unicode codepoints and pretty print them back.

### C

This project already includes the rules for the C programming language.

* `function_interspace` - number of blank between function definitions

* `indentation` - amount of indentation per level

	```haskell
		data Indentation = Tabs
		                 | Spaces Integer
	```

* `initial_indentation` - number of levels the whole file is indented with

* `single_statement_braces`

	```c
		// False
		while (condition)
			statement

		// True
		while (condition) {
			statement
		}
	```

* to do...

Coding standard of the project (as ironic as it might sound):
---
- tabs for indentation, spaces for alignment
- line ending is LF (multiple developers)
- HLint (every commit must generally pass the check)
- https://stackoverflow.com/questions/1983047/good-haskell-coding-standards