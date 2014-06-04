docstring-parser
================

A Common Lisp docstrings parser

- Library for parsing Common Lisp docstrings
- It is not a documentation generation system, nor is attached to a particular one. Just parses docstrings.
- Alternative syntaxes:
  - Lightweight/readable wiki-like syntax
  - Verbose/explicit syntax

Example:

(defun my-function (foo bar)
    "A //short// **description** goes here.
     
     Args:
        - foo (string): The **foo** argument
        - bar (integer): The **bar** argument
        
    An optional long description starts here.
    
    Look at **this** example:
    ``(my-function "foo" 22)``. 
    
    Also see: `my-func`(function)

    This is useful for:
        * Testing
        * Prototyping

    TODO: do this
    See: `person`(class), `print`(function)
    Tags: example, testing
    Categories: Printing
    Author: Mariano Montone"
    
    (print foo)
    (print bar))
