# DOCSTRING-PARSER

- Library for parsing Common Lisp documentation strings from functions, classes, packages, etc.
- It is not a documentation generation system, nor is attached to a particular one. Just parses docstrings.
- Alternative syntaxes:
  - Lightweight/readable wiki-like syntax
  - Verbose/explicit syntax

Example:

```lisp

(defun my-function (foo bar)
    "A //short// **description** goes here.
     
     Args:
        - foo (string): The **foo** argument.
        - bar (integer): The **bar** argument.

     Returns: the concatenation of **foo** and **bar**.
        
     An optional long description starts here.
    
     Look at **this** example:

     ``(my-function \"foo\" 22)``. 
    
     This is useful for:
        * Testing
        * Prototyping

     TODO: improve.
     See: `person`(class), `print`(function)
     Tags: example, testing
     Categories: Printing
     Author: Mariano Montone"
    
    (print foo)
    (print bar)
    (format nil "~A~A" foo bar))

```

## Functions
### arg-command-p

```lisp
(command)
```

Checks whether command is an **arg** command



**TODO**: Check that command options and arguments are valid.


### docstring-metadata-author

```lisp
(docstring-metadata)
```

Access docstring author



**Tags**: accessing, metadata
**Categories**: Metadata accessing


### docstring-metadata-categories

```lisp
(docstring-metadata)
```

Access docstring categories



**Tags**: accessing, metadata
**Categories**: Metadata accessing


### docstring-metadata-date

```lisp
(docstring-metadata)
```

Access docstring date



**Tags**: accessing, metadata
**Categories**: Metadata accessing


### docstring-metadata-see

```lisp
(docstring-metadata)
```

Access docstring see



**Tags**: accessing, metadata
**Categories**: Metadata accessing


### docstring-metadata-tags

```lisp
(docstring-metadata)
```

Access docstring tags



**Tags**: accessing, metadata
**Categories**: Metadata accessing


### docstring-metadata-todo

```lisp
(docstring-metadata)
```

Access docstring todo



**Tags**: accessing, metadata
**Categories**: Metadata accessing


### docstring-metadata-version

```lisp
(docstring-metadata)
```

Access docstring version



**Tags**: accessing, metadata
**Categories**: Metadata accessing


### parse-class-docstring

```lisp
(docstring)
```

Parses a class or structure docstring



**Returns**: a docstring or error

**Tags**: parsing
**Categories**: Parsing toplevel


### parse-class-slot-docstring

```lisp
(docstring)
```

Parses a class slot documentation string



**Returns**: a docstring or error

**Tags**: parsing
**Categories**: Parsing toplevel


### parse-function-docstring

```lisp
(docstring)
```

Parses a function docstring



**Returns**: a docstring or error

**Tags**: parsing
**Categories**: Parsing toplevel


### parse-package-docstring

```lisp
(docstring)
```

Parses a package docstring



**Returns**: a docstring or error

**Tags**: parsing
**Categories**: Parsing toplevel


### print-list-element

```lisp
(elem stream depth)
```

Print a list element



**Categories**: printing


### text

```lisp
(&rest esrap::arguments)
```

Arguments must be strings, or lists whose leaves are strings.
Catenates all the strings in arguments into a single string.





### valid-email-address-p

```lisp
(string)
```

Validates an email address

- **string**: (string) The string to validate


**Returns**: T if the email is valid

**Tags**: validation
**Categories**: utilities, validation


### valid-url-p

```lisp
(string)
```

Validates an url

- **string**: (string) The string to validate


**Returns**: T if the string is a valid url

**Tags**: validation
**Categories**: utilities, validation