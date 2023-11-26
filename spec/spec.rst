Typing Specification
====================

This document describes a specification for the Python type system.

The type system aims to provide a standard syntax for type annotations,
opening up Python code to easier static analysis and refactoring,
potential runtime type checking, and (perhaps, in some contexts)
code generation utilizing type information.

Of these goals, static analysis is the most important.  This includes
support for off-line type checkers such as mypy, as well as providing
a standard notation that can be used by IDEs for code completion and
refactoring.

This specification is organized in the following main sections:

TODO

.. _basics:

Basics of the type system
=========================

The meaning of annotations
--------------------------

Any function without annotations should be treated as having the most
general type possible, or ignored, by any type checker.

It is recommended but not required that checked functions have
annotations for all arguments and the return type.  For a checked
function, the default annotation for arguments and for the return type
is ``Any``.  An exception is the first argument of instance and
class methods. If it is not annotated, then it is assumed to have the
type of the containing class for instance methods, and a type object
type corresponding to the containing class object for class methods.
For example, in class ``A`` the first argument of an instance method
has the implicit type ``A``. In a class method, the precise type of
the first argument cannot be represented using the available type
notation.

(Note that the return type of ``__init__`` ought to be annotated with
``-> None``.  The reason for this is subtle.  If ``__init__`` assumed
a return annotation of ``-> None``, would that mean that an
argument-less, un-annotated ``__init__`` method should still be
type-checked?  Rather than leaving this ambiguous or introducing an
exception to the exception, we simply say that ``__init__`` ought to
have a return annotation; the default behavior is thus the same as for
other methods.)

A type checker is expected to check the body of a checked function for
consistency with the given annotations.  The annotations may also be 
used to check correctness of calls appearing in other checked functions.

Type checkers are expected to attempt to infer as much information as
necessary.  The minimum requirement is to handle the builtin
decorators ``@property``, ``@staticmethod`` and ``@classmethod``.

Type definition syntax
----------------------

The syntax leverages :pep:`3107`-style annotations with a number of
extensions described in sections below.  In its basic form, type
hinting is used by filling function annotation slots with classes::

  def greeting(name: str) -> str:
      return 'Hello ' + name

This states that the expected type of the ``name`` argument is
``str``.  Analogically, the expected return type is ``str``.

Expressions whose type is a subtype of a specific argument type are
also accepted for that argument.

Definition of terms
-------------------

This section defines a few terms that may be used elsewhere in the specification.

The definition of "MAY", "MUST", and "SHOULD", and "SHOULD NOT" are
to be interpreted as described in :rfc:`2119`.

"inline" - the types are part of the runtime code using :pep:`526` and
:pep:`3107` syntax (the filename ends in ``.py``).

"stubs" - files containing only type information, empty of runtime code
(the filename ends in ``.pyi``).

"Distributions" are the packaged files which are used to publish and distribute
a release. (:pep:`426`)

"Module" a file containing Python runtime code or stubbed type information.

"Package" a directory or directories that namespace Python modules.
(Note the distinction between packages and distributions.  While most
distributions are named after the one package they install, some
distributions install multiple packages.)

Acceptable type hints
---------------------

Type hints may be built-in classes (including those defined in
standard library or third-party extension modules), abstract base
classes, types available in the ``types`` module, and user-defined
classes (including those defined in the standard library or
third-party modules).

While annotations are normally the best format for type hints,
there are times when it is more appropriate to represent them
by a special comment, or in a separately distributed stub
file.  (See below for examples.)

Annotations must be valid expressions that evaluate without raising
exceptions at the time the function is defined (but see below for
forward references).

Annotations should be kept simple or static analysis tools may not be
able to interpret the values. For example, dynamically computed types
are unlikely to be understood.  (This is an
intentionally somewhat vague requirement; specific inclusions and
exclusions may be added in the future as warranted by the discussion.)

In addition to the above, the following special constructs defined
below may be used: ``None``, ``Any``, ``Union``, ``Tuple``,
``Callable``, all ABCs and stand-ins for concrete classes exported
from ``typing`` (e.g. ``Sequence`` and ``Dict``), type variables, and
type aliases.

Non-goals
---------

While the typing module contains some building blocks for
runtime type checking -- in particular the ``get_type_hints()``
function -- third party packages would have to be developed to
implement specific runtime type checking functionality, for example
using decorators or metaclasses.  Using type hints for performance
optimizations is left as an exercise for the reader.

It should also be emphasized that **Python will remain a dynamically
typed language, and there is no desire to ever make type hints
mandatory, even by convention.**

.. _type-system-features:

Type system features
====================

Using None
----------

When used in a type hint, the expression ``None`` is considered
equivalent to ``type(None)``.


Type aliases
------------

(See :pep:`613` for the introduction of ``TypeAlias``, and
:pep:`695` for the ``type`` statement.)

Type aliases may be defined by simple variable assignments::

  Url = str

  def retry(url: Url, retry_count: int) -> None: ...

Or by using ``typing.TypeAlias``::

  from typing import TypeAlias

  Url: TypeAlias = str

  def retry(url: Url, retry_count: int) -> None: ...

Or by using the ``type`` statement (Python 3.12 and higher)::

  type Url = str

  def retry(url: Url, retry_count: int) -> None: ...

Note that we recommend capitalizing alias names, since they represent
user-defined types, which (like user-defined classes) are typically
spelled that way.

Type aliases may be as complex as type hints in annotations --
anything that is acceptable as a type hint is acceptable in a type
alias::

    from typing import TypeVar
    from collections.abc import Iterable

    T = TypeVar('T', int, float, complex)
    Vector = Iterable[tuple[T, T]]

    def inproduct(v: Vector[T]) -> T:
        return sum(x*y for x, y in v)
    def dilate(v: Vector[T], scale: T) -> Vector[T]:
        return ((x * scale, y * scale) for x, y in v)
    vec: Vector[float] = []


This is equivalent to::

    from typing import TypeVar
    from collections.abc import Iterable

    T = TypeVar('T', int, float, complex)

    def inproduct(v: Iterable[tuple[T, T]]) -> T:
        return sum(x*y for x, y in v)
    def dilate(v: Iterable[tuple[T, T]], scale: T) -> Iterable[tuple[T, T]]:
        return ((x * scale, y * scale) for x, y in v)
    vec: Iterable[tuple[float, float]] = []

The explicit alias declaration syntax with ``TypeAlias`` clearly differentiates between the three
possible kinds of assignments: typed global expressions, untyped global
expressions, and type aliases. This avoids the existence of assignments that
break type checking when an annotation is added, and avoids classifying the
nature of the assignment based on the type of the value.

Implicit syntax (pre-existing):

::

  x = 1  # untyped global expression
  x: int = 1  # typed global expression

  x = int  # type alias
  x: Type[int] = int  # typed global expression


Explicit syntax:

::

  x = 1  # untyped global expression
  x: int = 1  # typed global expression

  x = int  # untyped global expression (see note below)
  x: Type[int] = int  # typed global expression

  x: TypeAlias = int  # type alias
  x: TypeAlias = "MyClass"  # type alias


Note: The examples above illustrate implicit and explicit alias declarations in
isolation. For the sake of backwards compatibility, type checkers should support
both simultaneously, meaning an untyped global expression ``x = int`` will
still be considered a valid type alias.

The ``type`` statement allows the creation of explicitly generic
type aliases::

  type ListOrSet[T] = list[T] | set[T]

Type parameters declared as part of a generic type alias are valid only
when evaluating the right-hand side of the type alias.

As with ``typing.TypeAlias``, type checkers should restrict the right-hand
expression to expression forms that are allowed within type annotations.
The use of more complex expression forms (call expressions, ternary operators,
arithmetic operators, comparison operators, etc.) should be flagged as an
error.

Type alias expressions are not allowed to use traditional type variables (i.e.
those allocated with an explicit ``TypeVar`` constructor call). Type checkers
should generate an error in this case.

::

    T = TypeVar("T")
    type MyList = list[T]  # Type checker error: traditional type variable usage


Callable
--------

Frameworks expecting callback functions of specific signatures might be
type hinted using ``Callable[[Arg1Type, Arg2Type], ReturnType]``.
Examples::

  from collections.abc import Callable

  def feeder(get_next_item: Callable[[], str]) -> None:
      # Body

  def async_query(on_success: Callable[[int], None],
                  on_error: Callable[[int, Exception], None]) -> None:
      # Body

It is possible to declare the return type of a callable without
specifying the call signature by substituting a literal ellipsis
(three dots) for the list of arguments::

  def partial(func: Callable[..., str], *args) -> Callable[..., str]:
      # Body

Note that there are no square brackets around the ellipsis.  The
arguments of the callback are completely unconstrained in this case
(and keyword arguments are acceptable).

Since using callbacks with keyword arguments is not perceived as a
common use case, there is currently no support for specifying keyword
arguments with ``Callable``.  Similarly, ``Callable`` does not support
specifying callback signatures with a variable number of arguments of a
specific type. For these use cases, see the section on
`Callback protocols`_.

Because ``typing.Callable`` does double-duty as a replacement for
``collections.abc.Callable``, ``isinstance(x, typing.Callable)`` is
implemented by deferring to ``isinstance(x, collections.abc.Callable)``.
However, ``isinstance(x, typing.Callable[...])`` is not supported.
