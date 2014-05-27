Ideas
=====

- Library for parsing Common Lisp docstrings
- It is not a documentation generation system, nor is attached to a particular one. Just parses docstrings.
- Alternative syntaxes:
  - Lightweight/readable wiki-like syntax
  - Verbose/explicit syntax
- Extensible

Syntax overview:
----------------

<short description>

Args: - arg1 (integer): The number of people.
      - arg2 (person): The person.
      - allow-p (boolean, default:t): Allow to do that.
      
Returns: T if found

<long description>

Author: Mariano Montone
Version: 1.0
Tags: checking, model
TODO: improve
Date: 5/5/2014
See also: name (reader), my-class (class), my-f (function), with-people (macro)
  
Descriptions markup
-------------------

**bold**   /italics/

Lists:

- Item1
- Item2

Source code: ``source code``

References: `my-function`(function)

Links: http://www.google.com(Google)

Verbose/explicit syntax:
------------------------

Verbose syntax should solve ambigueties that may pop up in the lighter syntax.

Look at Doxygen for ideas

Optional '\' and '@' for commands:

Experiment with sytaxes:

\link{http://www.google.com}{Google}
@link{http://www.google.com}{Google}
@param{my-arg}[type=integer, default=t]{This is my arg doc}
@see-also{my-f}{function}