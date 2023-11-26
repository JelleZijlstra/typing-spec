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


Generics
--------

Since type information about objects kept in containers cannot be
statically inferred in a generic way, abstract base classes have been
extended to support subscription to denote expected types for container
elements.  Example::

  from collections.abc import Mapping

  def notify_by_email(employees: set[Employee], overrides: Mapping[str, str]) -> None: ...

Generics can be parameterized by using a factory available in
``typing`` called ``TypeVar``.  Example::

  from collections.abc import Sequence
  from typing import TypeVar

  T = TypeVar('T')      # Declare type variable

  def first(l: Sequence[T]) -> T:   # Generic function
      return l[0]

Or, since Python 3.12 (:pep:`695`), by using the new syntax for
generic functions::

  from collections.abc import Sequence

  def first[T](l: Sequence[T]) -> T:   # Generic function
      return l[0]

The two syntaxes are equivalent.
In either case the contract is that the returned value is consistent with
the elements held by the collection.

A ``TypeVar()`` expression must always directly be assigned to a
variable (it should not be used as part of a larger expression).  The
argument to ``TypeVar()`` must be a string equal to the variable name
to which it is assigned.  Type variables must not be redefined.

``TypeVar`` supports constraining parametric types to a fixed set of possible
types (note: those types cannot be parameterized by type variables). For
example, we can define a type variable that ranges over just ``str`` and
``bytes``. By default, a type variable ranges over all possible types.
Example of constraining a type variable::

  from typing import TypeVar

  AnyStr = TypeVar('AnyStr', str, bytes)

  def concat(x: AnyStr, y: AnyStr) -> AnyStr:
      return x + y

Or using the built-in syntax (3.12 and higher)::

  def concat[AnyStr: (str, bytes)](x: AnyStr, y: AnyStr) -> AnyStr:
      return x + y

The function ``concat`` can be called with either two ``str`` arguments
or two ``bytes`` arguments, but not with a mix of ``str`` and ``bytes``
arguments.

There should be at least two constraints, if any; specifying a single
constraint is disallowed.

Subtypes of types constrained by a type variable should be treated
as their respective explicitly listed base types in the context of the
type variable.  Consider this example::

  class MyStr(str): ...

  x = concat(MyStr('apple'), MyStr('pie'))

The call is valid but the type variable ``AnyStr`` will be set to
``str`` and not ``MyStr``. In effect, the inferred type of the return
value assigned to ``x`` will also be ``str``.

Additionally, ``Any`` is a valid value for every type variable.
Consider the following::

  def count_truthy(elements: list[Any]) -> int:
      return sum(1 for elem in elements if elem)

This is equivalent to omitting the generic notation and just saying
``elements: list``.


User-defined generic types
--------------------------

You can include a ``Generic`` base class to define a user-defined class
as generic.  Example::

  from typing import TypeVar, Generic
  from logging import Logger

  T = TypeVar('T')

  class LoggedVar(Generic[T]):
      def __init__(self, value: T, name: str, logger: Logger) -> None:
          self.name = name
          self.logger = logger
          self.value = value

      def set(self, new: T) -> None:
          self.log('Set ' + repr(self.value))
          self.value = new

      def get(self) -> T:
          self.log('Get ' + repr(self.value))
          return self.value

      def log(self, message: str) -> None:
          self.logger.info('{}: {}'.format(self.name, message))

Or, in Python 3.12 and higher, by using the new syntax for generic
classes::

  class LoggedVar[T]:
      # methods as in previous example

This implicitly adds ``Generic[T]`` as a base class and type checkers
should treat the two largely equivalently (except for variance, see below).

``Generic[T]`` as a base class defines that the class ``LoggedVar``
takes a single type parameter ``T``. This also makes ``T`` valid as
a type within the class body.

The ``Generic`` base class uses a metaclass that defines ``__getitem__``
so that ``LoggedVar[t]`` is valid as a type::

  from collections.abc import Iterable

  def zero_all_vars(vars: Iterable[LoggedVar[int]]) -> None:
      for var in vars:
          var.set(0)

A generic type can have any number of type variables, and type variables
may be constrained. This is valid::

  from typing import TypeVar, Generic
  ...

  T = TypeVar('T')
  S = TypeVar('S')

  class Pair(Generic[T, S]):
      ...

Each type variable argument to ``Generic`` must be distinct. This is
thus invalid::

  from typing import TypeVar, Generic
  ...

  T = TypeVar('T')

  class Pair(Generic[T, T]):   # INVALID
      ...

The ``Generic[T]`` base class is redundant in simple cases where you
subclass some other generic class and specify type variables for its
parameters::

  from typing import TypeVar
  from collections.abc import Iterator

  T = TypeVar('T')

  class MyIter(Iterator[T]):
      ...

That class definition is equivalent to::

  class MyIter(Iterator[T], Generic[T]):
      ...

You can use multiple inheritance with ``Generic``::

  from typing import TypeVar, Generic
  from collections.abc import Sized, Iterable, Container

  T = TypeVar('T')

  class LinkedList(Sized, Generic[T]):
      ...

  K = TypeVar('K')
  V = TypeVar('V')

  class MyMapping(Iterable[tuple[K, V]],
                  Container[tuple[K, V]],
                  Generic[K, V]):
      ...

Subclassing a generic class without specifying type parameters assumes
``Any`` for each position.  In the following example, ``MyIterable``
is not generic but implicitly inherits from ``Iterable[Any]``::

  from collections.abc import Iterable

  class MyIterable(Iterable):  # Same as Iterable[Any]
      ...

Generic metaclasses are not supported.


Scoping rules for type variables
--------------------------------

Type variables follow normal name resolution rules.
However, there are some special cases in the static typechecking context:

* A type variable used in a generic function could be inferred to represent
  different types in the same code block. Example::

    from typing import TypeVar, Generic

    T = TypeVar('T')

    def fun_1(x: T) -> T: ...  # T here
    def fun_2(x: T) -> T: ...  # and here could be different

    fun_1(1)                   # This is OK, T is inferred to be int
    fun_2('a')                 # This is also OK, now T is str

* A type variable used in a method of a generic class that coincides
  with one of the variables that parameterize this class is always bound
  to that variable. Example::

    from typing import TypeVar, Generic

    T = TypeVar('T')

    class MyClass(Generic[T]):
        def meth_1(self, x: T) -> T: ...  # T here
        def meth_2(self, x: T) -> T: ...  # and here are always the same

    a: MyClass[int] = MyClass()
    a.meth_1(1)    # OK
    a.meth_2('a')  # This is an error!

* A type variable used in a method that does not match any of the variables
  that parameterize the class makes this method a generic function in that
  variable::

    T = TypeVar('T')
    S = TypeVar('S')
    class Foo(Generic[T]):
        def method(self, x: T, y: S) -> S:
            ...

    x: Foo[int] = Foo()
    y = x.method(0, "abc")  # inferred type of y is str

* Unbound type variables should not appear in the bodies of generic functions,
  or in the class bodies apart from method definitions::

    T = TypeVar('T')
    S = TypeVar('S')

    def a_fun(x: T) -> None:
        # this is OK
        y: list[T] = []
        # but below is an error!
        y: list[S] = []

    class Bar(Generic[T]):
        # this is also an error
        an_attr: list[S] = []

        def do_something(x: S) -> S:  # this is OK though
            ...

* A generic class definition that appears inside a generic function
  should not use type variables that parameterize the generic function::

    def a_fun(x: T) -> None:

        # This is OK
        a_list: list[T] = []
        ...

        # This is however illegal
        class MyGeneric(Generic[T]):
            ...

* A generic class nested in another generic class cannot use the same type
  variables. The scope of the type variables of the outer class
  doesn't cover the inner one::

    T = TypeVar('T')
    S = TypeVar('S')

    class Outer(Generic[T]):
        class Bad(Iterable[T]):       # Error
            ...
        class AlsoBad:
            x: list[T]  # Also an error

        class Inner(Iterable[S]):     # OK
            ...
        attr: Inner[T]  # Also OK


Instantiating generic classes and type erasure
----------------------------------------------

User-defined generic classes can be instantiated. Suppose we write
a ``Node`` class inheriting from ``Generic[T]``::

  from typing import TypeVar, Generic

  T = TypeVar('T')

  class Node(Generic[T]):
      ...

To create ``Node`` instances you call ``Node()`` just as for a regular
class.  At runtime the type (class) of the instance will be ``Node``.
But what type does it have to the type checker?  The answer depends on
how much information is available in the call.  If the constructor
(``__init__`` or ``__new__``) uses ``T`` in its signature, and a
corresponding argument value is passed, the type of the corresponding
argument(s) is substituted.  Otherwise, ``Any`` is assumed.  Example::

  from typing import TypeVar, Generic

  T = TypeVar('T')

  class Node(Generic[T]):
      x: T # Instance attribute (see below)
      def __init__(self, label: T = None) -> None:
          ...

  x = Node('')  # Inferred type is Node[str]
  y = Node(0)   # Inferred type is Node[int]
  z = Node()    # Inferred type is Node[Any]

In case the inferred type uses ``[Any]`` but the intended type is more
specific, you can use a type comment (see below) to force the type of
the variable, e.g.::

  # (continued from previous example)
  a: Node[int] = Node()
  b: Node[str] = Node()

Alternatively, you can instantiate a specific concrete type, e.g.::

  # (continued from previous example)
  p = Node[int]()
  q = Node[str]()
  r = Node[int]('')  # Error
  s = Node[str](0)   # Error

Note that the runtime type (class) of ``p`` and ``q`` is still just ``Node``
-- ``Node[int]`` and ``Node[str]`` are distinguishable class objects, but
the runtime class of the objects created by instantiating them doesn't
record the distinction. This behavior is called "type erasure"; it is
common practice in languages with generics (e.g. Java, TypeScript).

Using generic classes (parameterized or not) to access attributes will result
in type check failure. Outside the class definition body, a class attribute
cannot be assigned, and can only be looked up by accessing it through a
class instance that does not have an instance attribute with the same name::

  # (continued from previous example)
  Node[int].x = 1  # Error
  Node[int].x      # Error
  Node.x = 1       # Error
  Node.x           # Error
  type(p).x        # Error
  p.x              # Ok (evaluates to None)
  Node[int]().x    # Ok (evaluates to None)
  p.x = 1          # Ok, but assigning to instance attribute

Generic versions of abstract collections like ``Mapping`` or ``Sequence``
and generic versions of built-in classes -- ``List``, ``Dict``, ``Set``,
and ``FrozenSet`` -- cannot be instantiated. However, concrete user-defined
subclasses thereof and generic versions of concrete collections can be
instantiated::

  data = DefaultDict[int, bytes]()

Note that one should not confuse static types and runtime classes.
The type is still erased in this case and the above expression is
just a shorthand for::

  data: DefaultDict[int, bytes] = collections.defaultdict()

It is not recommended to use the subscripted class (e.g. ``Node[int]``)
directly in an expression -- using a type alias (e.g. ``IntNode = Node[int]``)
instead is preferred. (First, creating the subscripted class,
e.g. ``Node[int]``, has a runtime cost. Second, using a type alias
is more readable.)


Arbitrary generic types as base classes
---------------------------------------

``Generic[T]`` is only valid as a base class -- it's not a proper type.
However, user-defined generic types such as ``LinkedList[T]`` from the
above example and built-in generic types and ABCs such as ``list[T]``
and ``Iterable[T]`` are valid both as types and as base classes. For
example, we can define a subclass of ``dict`` that specializes type
arguments::

  class Node:
      ...

  class SymbolTable(dict[str, list[Node]]):
      def push(self, name: str, node: Node) -> None:
          self.setdefault(name, []).append(node)

      def pop(self, name: str) -> Node:
          return self[name].pop()

      def lookup(self, name: str) -> Node | None:
          nodes = self.get(name)
          if nodes:
              return nodes[-1]
          return None

``SymbolTable`` is a subclass of ``dict`` and a subtype of ``dict[str,
list[Node]]``.

If a generic base class has a type variable as a type argument, this
makes the defined class generic. For example, we can define a generic
``LinkedList`` class that is iterable and a container::

  from typing import TypeVar
  from collections.abc import Iterable, Container

  T = TypeVar('T')

  class LinkedList(Iterable[T], Container[T]):
      ...

Now ``LinkedList[int]`` is a valid type. Note that we can use ``T``
multiple times in the base class list, as long as we don't use the
same type variable ``T`` multiple times within ``Generic[...]``.

Also consider the following example::

  from typing import TypeVar
  from collections.abc import Mapping

  T = TypeVar('T')

  class MyDict(Mapping[str, T]):
      ...

In this case MyDict has a single parameter, T.


Abstract generic types
----------------------

The metaclass used by ``Generic`` is a subclass of ``abc.ABCMeta``.
A generic class can be an ABC by including abstract methods
or properties, and generic classes can also have ABCs as base
classes without a metaclass conflict.


Type variables with an upper bound
----------------------------------

A type variable may specify an upper bound using ``bound=<type>`` (note:
<type> itself cannot be parameterized by type variables). This means that an
actual type substituted (explicitly or implicitly) for the type variable must
be a subtype of the boundary type. Example::

  from typing import TypeVar
  from collections.abc import Sized

  ST = TypeVar('ST', bound=Sized)

  def longer(x: ST, y: ST) -> ST:
      if len(x) > len(y):
          return x
      else:
          return y

  longer([1], [1, 2])  # ok, return type list[int]
  longer({1}, {1, 2})  # ok, return type set[int]
  longer([1], {1, 2})  # ok, return type Collection[int]

An upper bound cannot be combined with type constraints (as used in
``AnyStr``, see the example earlier); type constraints cause the
inferred type to be _exactly_ one of the constraint types, while an
upper bound just requires that the actual type is a subtype of the
boundary type.


Covariance and contravariance
-----------------------------

Consider a class ``Employee`` with a subclass ``Manager``.  Now
suppose we have a function with an argument annotated with
``list[Employee]``.  Should we be allowed to call this function with a
variable of type ``list[Manager]`` as its argument?  Many people would
answer "yes, of course" without even considering the consequences.
But unless we know more about the function, a type checker should
reject such a call: the function might append an ``Employee`` instance
to the list, which would violate the variable's type in the caller.

It turns out such an argument acts *contravariantly*, whereas the
intuitive answer (which is correct in case the function doesn't mutate
its argument!) requires the argument to act *covariantly*.  A longer
introduction to these concepts can be found on `Wikipedia
<https://en.wikipedia.org/wiki/Covariance_and_contravariance_%28computer_science%29>`_ and in :pep:`483`; here we just show how to control
a type checker's behavior.

By default generic types declared using the old ``TypeVar`` syntax
are considered *invariant* in all type variables,
which means that values for variables annotated with types like
``list[Employee]`` must exactly match the type annotation -- no subclasses or
superclasses of the type parameter (in this example ``Employee``) are
allowed. See below for the behavior when using the built-in generic syntax
in Python 3.12 and higher.

To facilitate the declaration of container types where covariant or
contravariant type checking is acceptable, type variables accept keyword
arguments ``covariant=True`` or ``contravariant=True``. At most one of these
may be passed. Generic types defined with such variables are considered
covariant or contravariant in the corresponding variable. By convention,
it is recommended to use names ending in ``_co`` for type variables
defined with ``covariant=True`` and names ending in ``_contra`` for that
defined with ``contravariant=True``.

A typical example involves defining an immutable (or read-only)
container class::

  from typing import TypeVar, Generic
  from collections.abc import Iterable, Iterator

  T_co = TypeVar('T_co', covariant=True)

  class ImmutableList(Generic[T_co]):
      def __init__(self, items: Iterable[T_co]) -> None: ...
      def __iter__(self) -> Iterator[T_co]: ...
      ...

  class Employee: ...

  class Manager(Employee): ...

  def dump_employees(emps: ImmutableList[Employee]) -> None:
      for emp in emps:
          ...

  mgrs: ImmutableList[Manager] = ImmutableList([Manager()])
  dump_employees(mgrs)  # OK

The read-only collection classes in ``typing`` are all declared
covariant in their type variable (e.g. ``Mapping`` and ``Sequence``). The
mutable collection classes (e.g. ``MutableMapping`` and
``MutableSequence``) are declared invariant. The one example of
a contravariant type is the ``Generator`` type, which is contravariant
in the ``send()`` argument type (see below).

Note: Covariance or contravariance is *not* a property of a type variable,
but a property of a generic class defined using this variable.
Variance is only applicable to generic types; generic functions
do not have this property. The latter should be defined using only
type variables without ``covariant`` or ``contravariant`` keyword arguments.
For example, the following example is
fine::

  from typing import TypeVar

  class Employee: ...

  class Manager(Employee): ...

  E = TypeVar('E', bound=Employee)

  def dump_employee(e: E) -> None: ...

  dump_employee(Manager())  # OK

while the following is prohibited::

  B_co = TypeVar('B_co', covariant=True)

  def bad_func(x: B_co) -> B_co:  # Flagged as error by a type checker
      ...

Variance Inference
------------------

(Originally specified by :pep:`695`.)

The introduction of explicit syntax for generic classes in Python 3.12
eliminates the need for variance to be specified for type
parameters. Instead, type checkers will infer the variance of type parameters
based on their usage within a class. Type parameters are inferred to be
invariant, covariant, or contravariant depending on how they are used.

Python type checkers already include the ability to determine the variance of
type parameters for the purpose of validating variance within a generic
protocol class. This capability can be used for all classes (whether or not
they are protocols) to calculate the variance of each type parameter.

The algorithm for computing the variance of a type parameter is as follows.

For each type parameter in a generic class:

1. If the type parameter is variadic (``TypeVarTuple``) or a parameter
specification (``ParamSpec``), it is always considered invariant. No further
inference is needed.

2. If the type parameter comes from a traditional ``TypeVar`` declaration and
is not specified as ``infer_variance`` (see below), its variance is specified
by the ``TypeVar`` constructor call. No further inference is needed.

3. Create two specialized versions of the class. We'll refer to these as
``upper`` and ``lower`` specializations. In both of these specializations,
replace all type parameters other than the one being inferred by a dummy type
instance (a concrete anonymous class that is type compatible with itself and
assumed to meet the bounds or constraints of the type parameter). In
the ``upper`` specialized class, specialize the target type parameter with
an ``object`` instance. This specialization ignores the type parameter's
upper bound or constraints. In the ``lower`` specialized class, specialize
the target type parameter with itself (i.e. the corresponding type argument
is the type parameter itself).

4. Determine whether ``lower`` can be assigned to ``upper`` using normal type
compatibility rules. If so, the target type parameter is covariant. If not,
determine whether ``upper`` can be assigned to ``lower``. If so, the target
type parameter is contravariant. If neither of these combinations are
assignable, the target type parameter is invariant.

Here is an example.

::

    class ClassA[T1, T2, T3](list[T1]):
        def method1(self, a: T2) -> None:
            ...
        
        def method2(self) -> T3:
            ...

To determine the variance of ``T1``, we specialize ``ClassA`` as follows:

::

    upper = ClassA[object, Dummy, Dummy]
    lower = ClassA[T1, Dummy, Dummy]

We find that ``upper`` is not assignable to ``lower`` using normal type
compatibility rules defined in :pep:`484`. Likewise, ``lower`` is not assignable
to ``upper``, so we conclude that ``T1`` is invariant.

To determine the variance of ``T2``, we specialize ``ClassA`` as follows:

::

    upper = ClassA[Dummy, object, Dummy]
    lower = ClassA[Dummy, T2, Dummy]

Since ``upper`` is assignable to ``lower``, ``T2`` is contravariant.

To determine the variance of ``T3``, we specialize ``ClassA`` as follows:

::

    upper = ClassA[Dummy, Dummy, object]
    lower = ClassA[Dummy, Dummy, T3]

Since ``lower`` is assignable to ``upper``, ``T3`` is covariant.


Auto Variance For TypeVar
^^^^^^^^^^^^^^^^^^^^^^^^^

The existing ``TypeVar`` class constructor accepts keyword parameters named
``covariant`` and ``contravariant``. If both of these are ``False``, the
type variable is assumed to be invariant. PEP 695 adds another keyword
parameter named ``infer_variance`` indicating that a type checker should use
inference to determine whether the type variable is invariant, covariant or
contravariant. A corresponding instance variable ``__infer_variance__`` can be
accessed at runtime to determine whether the variance is inferred. Type
variables that are implicitly allocated using the new syntax will always
have ``__infer_variance__`` set to ``True``.

A generic class that uses the traditional syntax may include combinations of
type variables with explicit and inferred variance.

::

    T1 = TypeVar("T1", infer_variance=True)  # Inferred variance
    T2 = TypeVar("T2")  # Invariant
    T3 = TypeVar("T3", covariant=True)  # Covariant

    # A type checker should infer the variance for T1 but use the
    # specified variance for T2 and T3.
    class ClassA(Generic[T1, T2, T3]): ...


Compatibility with Traditional TypeVars
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The existing mechanism for allocating ``TypeVar``, ``TypeVarTuple``, and
``ParamSpec`` is retained for backward compatibility. However, these
"traditional" type variables should not be combined with type parameters
allocated using the new syntax. Such a combination should be flagged as
an error by type checkers. This is necessary because the type parameter
order is ambiguous.

It is OK to combine traditional type variables with new-style type parameters
if the class, function, or type alias does not use the new syntax. The
new-style type parameters must come from an outer scope in this case.

::

    K = TypeVar("K")

    class ClassA[V](dict[K, V]): ...  # Type checker error

    class ClassB[K, V](dict[K, V]): ...  # OK

    class ClassC[V]:
        # The use of K and V for "method1" is OK because it uses the
        # "traditional" generic function mechanism where type parameters
        # are implicit. In this case V comes from an outer scope (ClassC)
        # and K is introduced implicitly as a type parameter for "method1".
        def method1(self, a: V, b: K) -> V | K: ...

        # The use of M and K are not allowed for "method2". A type checker
        # should generate an error in this case because this method uses the
        # new syntax for type parameters, and all type parameters associated
        # with the method must be explicitly declared. In this case, ``K``
        # is not declared by "method2", nor is it supplied by a new-style
        # type parameter defined in an outer scope.
        def method2[M](self, a: M, b: K) -> M | K: ...


Forward references
------------------

When a type hint contains names that have not been defined yet, that
definition may be expressed as a string literal, to be resolved later.

A situation where this occurs commonly is the definition of a
container class, where the class being defined occurs in the signature
of some of the methods.  For example, the following code (the start of
a simple binary tree implementation) does not work::

  class Tree:
      def __init__(self, left: Tree, right: Tree):
          self.left = left
          self.right = right

To address this, we write::

  class Tree:
      def __init__(self, left: 'Tree', right: 'Tree'):
          self.left = left
          self.right = right

The string literal should contain a valid Python expression (i.e.,
``compile(lit, '', 'eval')`` should be a valid code object) and it
should evaluate without errors once the module has been fully loaded.
The local and global namespace in which it is evaluated should be the
same namespaces in which default arguments to the same function would
be evaluated.

Moreover, the expression should be parseable as a valid type hint, i.e.,
it is constrained by the rules from the section `Acceptable type hints`_
above.

It is allowable to use string literals as *part* of a type hint, for
example::

    class Tree:
        ...
        def leaves(self) -> list['Tree']:
            ...

A common use for forward references is when e.g. Django models are
needed in the signatures.  Typically, each model is in a separate
file, and has methods taking arguments whose type involves other models.
Because of the way circular imports work in Python, it is often not
possible to import all the needed models directly::

    # File models/a.py
    from models.b import B
    class A(Model):
        def foo(self, b: B): ...

    # File models/b.py
    from models.a import A
    class B(Model):
        def bar(self, a: A): ...

    # File main.py
    from models.a import A
    from models.b import B

Assuming main is imported first, this will fail with an ImportError at
the line ``from models.a import A`` in models/b.py, which is being
imported from models/a.py before a has defined class A.  The solution
is to switch to module-only imports and reference the models by their
_module_._class_ name::

    # File models/a.py
    from models import b
    class A(Model):
        def foo(self, b: 'b.B'): ...

    # File models/b.py
    from models import a
    class B(Model):
        def bar(self, a: 'a.A'): ...

    # File main.py
    from models.a import A
    from models.b import B


Union types
-----------

Since accepting a small, limited set of expected types for a single
argument is common, the type system supports union types, created with the
``|`` operator.
Example::

  def handle_employees(e: Employee | Sequence[Employee]) -> None:
      if isinstance(e, Employee):
          e = [e]
      ...

A type factored by ``T1 | T2 | ...`` is a supertype
of all types ``T1``, ``T2``, etc., so that a value that
is a member of one of these types is acceptable for an argument
annotated by ``T1 | T2 | ...``.

One common case of union types are *optional* types.  By default,
``None`` is an invalid value for any type, unless a default value of
``None`` has been provided in the function definition.  Examples::

  def handle_employee(e: Employee | None) -> None: ...

A past version of this specification allowed type checkers to assume an optional
type when the default value is ``None``, as in this code::

  def handle_employee(e: Employee = None): ...

This would have been treated as equivalent to::

  def handle_employee(e: Employee | None = None) -> None: ...

This is no longer the recommended behavior. Type checkers should move
towards requiring the optional type to be made explicit.

Support for singleton types in unions
-------------------------------------

A singleton instance is frequently used to mark some special condition,
in particular in situations where ``None`` is also a valid value
for a variable. Example::

  _empty = object()

  def func(x=_empty):
      if x is _empty:  # default argument value
          return 0
      elif x is None:  # argument was provided and it's None
          return 1
      else:
          return x * 2

To allow precise typing in such situations, the user should use
a union type in conjunction with the ``enum.Enum`` class provided
by the standard library, so that type errors can be caught statically::

  from enum import Enum

  class Empty(Enum):
      token = 0
  _empty = Empty.token

  def func(x: int | None | Empty = _empty) -> int:

      boom = x * 42  # This fails type check

      if x is _empty:
          return 0
      elif x is None:
          return 1
      else:  # At this point typechecker knows that x can only have type int
          return x * 2

Since the subclasses of ``Enum`` cannot be further subclassed,
the type of variable ``x`` can be statically inferred in all branches
of the above example. The same approach is applicable if more than one
singleton object is needed: one can use an enumeration that has more than
one value::

  class Reason(Enum):
      timeout = 1
      error = 2

  def process(response: str | Reason = '') -> str:
      if response is Reason.timeout:
          return 'TIMEOUT'
      elif response is Reason.error:
          return 'ERROR'
      else:
          # response can be only str, all other possible values exhausted
          return 'PROCESSED: ' + response


The ``Any`` type
----------------

A special kind of type is ``Any``.  Every type is consistent with
``Any``.  It can be considered a type that has all values and all methods.
Note that ``Any`` and builtin type ``object`` are completely different.

When the type of a value is ``object``, the type checker will reject
almost all operations on it, and assigning it to a variable (or using
it as a return value) of a more specialized type is a type error.  On
the other hand, when a value has type ``Any``, the type checker will
allow all operations on it, and a value of type ``Any`` can be assigned
to a variable (or used as a return value) of a more constrained type.

A function parameter without an annotation is assumed to be annotated with
``Any``. If a generic type is used without specifying type parameters,
they are assumed to be ``Any``::

  from collections.abc import Mapping

  def use_map(m: Mapping) -> None:  # Same as Mapping[Any, Any]
      ...

This rule also applies to ``tuple``, in annotation context it is equivalent
to ``tuple[Any, ...]``. As well, a bare
``Callable`` in an annotation is equivalent to ``Callable[..., Any]``::

  from collections.abc import Callable

  def check_args(args: tuple) -> bool:
      ...

  check_args(())           # OK
  check_args((42, 'abc'))  # Also OK
  check_args(3.14)         # Flagged as error by a type checker

  # A list of arbitrary callables is accepted by this function
  def apply_callbacks(cbs: list[Callable]) -> None:
      ...

``Any`` can also be used as a base class. This can be useful for
avoiding type checker errors with classes that can duck type anywhere or
are highly dynamic.


The ``NoReturn`` type
---------------------

The ``typing`` module provides a special type ``NoReturn`` to annotate functions
that never return normally. For example, a function that unconditionally
raises an exception::

  from typing import NoReturn

  def stop() -> NoReturn:
      raise RuntimeError('no way')

The ``NoReturn`` annotation is used for functions such as ``sys.exit``.
Static type checkers will ensure that functions annotated as returning
``NoReturn`` truly never return, either implicitly or explicitly::

  import sys
  from typing import NoReturn

    def f(x: int) -> NoReturn:  # Error, f(0) implicitly returns None
        if x != 0:
            sys.exit(1)

The checkers will also recognize that the code after calls to such functions
is unreachable and will behave accordingly::

  # continue from first example
  def g(x: int) -> int:
      if x > 0:
          return x
      stop()
      return 'whatever works'  # Error might be not reported by some checkers
                               # that ignore errors in unreachable blocks

The ``NoReturn`` type is only valid as a return annotation of functions,
and considered an error if it appears in other positions::

  from typing import NoReturn

  # All of the following are errors
  def bad1(x: NoReturn) -> int:
      ...
  bad2: NoReturn = None
  def bad3() -> list[NoReturn]:
      ...


The ``Never`` type
------------------

Since Python 3.11, the ``typing`` module has a primitive ``Never``. This
represents the bottom type, a type that has no members. Type checkers are
expected to treat this type as equivalent to ``NoReturn``, but it is explicitly
also allowed in argument positions.


The type of class objects
-------------------------

Sometimes you want to talk about class objects, in particular class
objects that inherit from a given class.  This can be spelled as
``type[C]`` where ``C`` is a class.  To clarify: while ``C`` (when
used as an annotation) refers to instances of class ``C``, ``type[C]``
refers to *subclasses* of ``C``.  (This is a similar distinction as
between ``object`` and ``type``.)

For example, suppose we have the following classes::

  class User: ...  # Abstract base for User classes
  class BasicUser(User): ...
  class ProUser(User): ...
  class TeamUser(User): ...

And suppose we have a function that creates an instance of one of
these classes if you pass it a class object::

  def new_user(user_class):
      user = user_class()
      # (Here we could write the user object to a database)
      return user

Without subscripting ``type[]`` the best we could do to annotate ``new_user()``
would be::

  def new_user(user_class: type) -> User:
      ...

However using ``type[]`` and a type variable with an upper bound we
can do much better::

  U = TypeVar('U', bound=User)
  def new_user(user_class: type[U]) -> U:
      ...

Now when we call ``new_user()`` with a specific subclass of ``User`` a
type checker will infer the correct type of the result::

  joe = new_user(BasicUser)  # Inferred type is BasicUser

The value corresponding to ``type[C]`` must be an actual class object
that's a subtype of ``C``, not a special form.  In other words, in the
above example calling e.g. ``new_user(BasicUser | ProUser)`` is
rejected by the type checker (in addition to failing at runtime
because you can't instantiate a union).

Note that it is legal to use a union of classes as the parameter for
``type[]``, as in::

  def new_non_team_user(user_class: type[BasicUser | ProUser]):
      user = new_user(user_class)
      ...

However the actual argument passed in at runtime must still be a
concrete class object, e.g. in the above example::

  new_non_team_user(ProUser)  # OK
  new_non_team_user(TeamUser)  # Disallowed by type checker

``type[Any]`` is also supported (see below for its meaning).

``type[T]`` where ``T`` is a type variable is allowed when annotating the
first argument of a class method (see the relevant section).

Any other special constructs like ``tuple`` or ``Callable`` are not allowed
as an argument to ``type``.

There are some concerns with this feature: for example when
``new_user()`` calls ``user_class()`` this implies that all subclasses
of ``User`` must support this in their constructor signature.  However
this is not unique to ``type[]``: class methods have similar concerns.
A type checker ought to flag violations of such assumptions, but by
default constructor calls that match the constructor signature in the
indicated base class (``User`` in the example above) should be
allowed.  A program containing a complex or extensible class hierarchy
might also handle this by using a factory class method.

When ``type`` is parameterized it requires exactly one parameter.
Plain ``type`` without brackets, the root of Python's metaclass
hierarchy, is equivalent to ``type[Any]``.

Regarding the behavior of ``type[Any]`` (or ``type``),
accessing attributes of a variable with this type only provides
attributes and methods defined by ``type`` (for example,
``__repr__()`` and ``__mro__``).  Such a variable can be called with
arbitrary arguments, and the return type is ``Any``.

``type`` is covariant in its parameter, because ``type[Derived]`` is a
subtype of ``type[Base]``::

  def new_pro_user(pro_user_class: type[ProUser]):
      user = new_user(pro_user_class)  # OK
      ...


Annotating instance and class methods
-------------------------------------

In most cases the first argument of class and instance methods
does not need to be annotated, and it is assumed to have the
type of the containing class for instance methods, and a type object
type corresponding to the containing class object for class methods.
In addition, the first argument in an instance method can be annotated
with a type variable. In this case the return type may use the same
type variable, thus making that method a generic function. For example::

  T = TypeVar('T', bound='Copyable')
  class Copyable:
      def copy(self: T) -> T:
          # return a copy of self

  class C(Copyable): ...
  c = C()
  c2 = c.copy()  # type here should be C

The same applies to class methods using ``Type[]`` in an annotation
of the first argument::

  T = TypeVar('T', bound='C')
  class C:
      @classmethod
      def factory(cls: Type[T]) -> T:
          # make a new instance of cls

  class D(C): ...
  d = D.factory()  # type here should be D

Note that some type checkers may apply restrictions on this use, such as
requiring an appropriate upper bound for the type variable used
(see examples).


Version and platform checking
-----------------------------

Type checkers are expected to understand simple version and platform
checks, e.g.::

  import sys

  if sys.version_info[0] >= 3:
      # Python 3 specific definitions
  else:
      # Python 2 specific definitions

  if sys.platform == 'win32':
      # Windows specific definitions
  else:
      # Posix specific definitions

Don't expect a checker to understand obfuscations like
``"".join(reversed(sys.platform)) == "xunil"``.

``ClassVar``
------------

(Originally specified in :pep:`526`.)

A covariant type ``ClassVar[T_co]`` exists in the ``typing``
module. It accepts only a single argument that should be a valid type,
and is used to annotate class variables that should not be set on class
instances. This restriction is ensured by static checkers,
but not at runtime.

Type annotations can be used to annotate class and instance variables
in class bodies and methods. In particular, the value-less notation ``a: int``
allows one to annotate instance variables that should be initialized
in ``__init__`` or ``__new__``. The syntax is as follows::

  class BasicStarship:
      captain: str = 'Picard'               # instance variable with default
      damage: int                           # instance variable without default
      stats: ClassVar[dict[str, int]] = {}  # class variable

Here ``ClassVar`` is a special class defined by the typing module that
indicates to the static type checker that this variable should not be
set on instances.

Note that a ``ClassVar`` parameter cannot include any type variables, regardless
of the level of nesting: ``ClassVar[T]`` and ``ClassVar[list[set[T]]]`` are
both invalid if ``T`` is a type variable.

This could be illustrated with a more detailed example. In this class::

  class Starship:
      captain = 'Picard'
      stats = {}

      def __init__(self, damage, captain=None):
          self.damage = damage
          if captain:
              self.captain = captain  # Else keep the default

      def hit(self):
          Starship.stats['hits'] = Starship.stats.get('hits', 0) + 1

``stats`` is intended to be a class variable (keeping track of many different
per-game statistics), while ``captain`` is an instance variable with a default
value set in the class. This difference might not be seen by a type
checker: both get initialized in the class, but ``captain`` serves only
as a convenient default value for the instance variable, while ``stats``
is truly a class variable -- it is intended to be shared by all instances.

Since both variables happen to be initialized at the class level, it is
useful to distinguish them by marking class variables as annotated with
types wrapped in ``ClassVar[...]``. In this way a type checker may flag
accidental assignments to attributes with the same name on instances.

For example, annotating the discussed class::

  class Starship:
      captain: str = 'Picard'
      damage: int
      stats: ClassVar[dict[str, int]] = {}

      def __init__(self, damage: int, captain: str = None):
          self.damage = damage
          if captain:
              self.captain = captain  # Else keep the default

      def hit(self):
          Starship.stats['hits'] = Starship.stats.get('hits', 0) + 1

  enterprise_d = Starship(3000)
  enterprise_d.stats = {} # Flagged as error by a type checker
  Starship.stats = {} # This is OK

As a matter of convenience (and convention), instance variables can be
annotated in ``__init__`` or other methods, rather than in the class::

  from typing import Generic, TypeVar
  T = TypeVar('T')

  class Box(Generic[T]):
      def __init__(self, content):
          self.content: T = content



Protocols
---------

(Originally specified in :pep:`544`.)

Terminology
^^^^^^^^^^^

The term *protocols* is used for types supporting structural
subtyping. The reason is that the term *iterator protocol*,
for example, is widely understood in the community, and coming up with
a new term for this concept in a statically typed context would just create
confusion.

This has the drawback that the term *protocol* becomes overloaded with
two subtly different meanings: the first is the traditional, well-known but
slightly fuzzy concept of protocols such as iterator; the second is the more
explicitly defined concept of protocols in statically typed code.
The distinction is not important most of the time, and in other
cases we can just add a qualifier such as *protocol classes*
when referring to the static type concept.

If a class includes a protocol in its MRO, the class is called
an *explicit* subclass of the protocol. If a class is a structural subtype
of a protocol, it is said to implement the protocol and to be compatible
with a protocol. If a class is compatible with a protocol but the protocol
is not included in the MRO, the class is an *implicit* subtype
of the protocol. (Note that one can explicitly subclass a protocol and
still not implement it if a protocol attribute is set to ``None``
in the subclass, see Python `data model <https://docs.python.org/3/reference/datamodel.html#special-method-names>`_
for details.)

The attributes (variables and methods) of a protocol that are mandatory
for another class in order to be considered a structural subtype are called
protocol members.


.. _definition:

Defining a protocol
^^^^^^^^^^^^^^^^^^^

Protocols are defined by including a special new class ``typing.Protocol``
(an instance of ``abc.ABCMeta``) in the base classes list, typically
at the end of the list. Here is a simple example::

  from typing import Protocol

  class SupportsClose(Protocol):
      def close(self) -> None:
          ...

Now if one defines a class ``Resource`` with a ``close()`` method that has
a compatible signature, it would implicitly be a subtype of
``SupportsClose``, since the structural subtyping is used for
protocol types::

  class Resource:
      ...
      def close(self) -> None:
          self.file.close()
          self.lock.release()

Apart from a few restrictions explicitly mentioned below, protocol types can
be used in every context where normal types can::

  def close_all(things: Iterable[SupportsClose]) -> None:
      for t in things:
          t.close()

  f = open('foo.txt')
  r = Resource()
  close_all([f, r])  # OK!
  close_all([1])     # Error: 'int' has no 'close' method

Note that both the user-defined class ``Resource`` and the built-in
``IO`` type (the return type of ``open()``) are considered subtypes of
``SupportsClose``, because they provide a ``close()`` method with
a compatible type signature.


Protocol members
^^^^^^^^^^^^^^^^

All methods defined in the protocol class body are protocol members, both
normal and decorated with ``@abstractmethod``. If any parameters of a
protocol method are not annotated, then their types are assumed to be ``Any``
(see :pep:`484`). Bodies of protocol methods are type checked.
An abstract method that should not be called via ``super()`` ought to raise
``NotImplementedError``. Example::

  from typing import Protocol
  from abc import abstractmethod

  class Example(Protocol):
      def first(self) -> int:     # This is a protocol member
          return 42

      @abstractmethod
      def second(self) -> int:    # Method without a default implementation
          raise NotImplementedError

Static methods, class methods, and properties are equally allowed
in protocols.

To define a protocol variable, one can use :pep:`526` variable
annotations in the class body. Additional attributes *only* defined in
the body of a method by assignment via ``self`` are not allowed. The rationale
for this is that the protocol class implementation is often not shared by
subtypes, so the interface should not depend on the default implementation.
Examples::

  from typing import Protocol

  class Template(Protocol):
      name: str        # This is a protocol member
      value: int = 0   # This one too (with default)

      def method(self) -> None:
          self.temp: list[int] = [] # Error in type checker

  class Concrete:
      def __init__(self, name: str, value: int) -> None:
          self.name = name
          self.value = value

      def method(self) -> None:
          return

  var: Template = Concrete('value', 42)  # OK

To distinguish between protocol class variables and protocol instance
variables, the special ``ClassVar`` annotation should be used as specified
by :pep:`526`. By default, protocol variables as defined above are considered
readable and writable. To define a read-only protocol variable, one can use
an (abstract) property.


Explicitly declaring implementation
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

To explicitly declare that a certain class implements a given protocol,
it can be used as a regular base class. In this case a class could use
default implementations of protocol members. Static analysis tools are
expected to automatically detect that a class implements a given protocol.
So while it's possible to subclass a protocol explicitly, it's *not necessary*
to do so for the sake of type-checking.

The default implementations cannot be used if
the subtype relationship is implicit and only via structural
subtyping -- the semantics of inheritance is not changed. Examples::

    class PColor(Protocol):
        @abstractmethod
        def draw(self) -> str:
            ...
        def complex_method(self) -> int:
            # some complex code here

    class NiceColor(PColor):
        def draw(self) -> str:
            return "deep blue"

    class BadColor(PColor):
        def draw(self) -> str:
            return super().draw()  # Error, no default implementation

    class ImplicitColor:   # Note no 'PColor' base here
        def draw(self) -> str:
            return "probably gray"
        def complex_method(self) -> int:
            # class needs to implement this

    nice: NiceColor
    another: ImplicitColor

    def represent(c: PColor) -> None:
        print(c.draw(), c.complex_method())

    represent(nice) # OK
    represent(another) # Also OK

Note that there is little difference between explicit and implicit
subtypes; the main benefit of explicit subclassing is to get some protocol
methods "for free". In addition, type checkers can statically verify that
the class actually implements the protocol correctly::

    class RGB(Protocol):
        rgb: tuple[int, int, int]

        @abstractmethod
        def intensity(self) -> int:
            return 0

    class Point(RGB):
        def __init__(self, red: int, green: int, blue: str) -> None:
            self.rgb = red, green, blue  # Error, 'blue' must be 'int'

        # Type checker might warn that 'intensity' is not defined

A class can explicitly inherit from multiple protocols and also from normal
classes. In this case methods are resolved using normal MRO and a type checker
verifies that all subtyping are correct. The semantics of ``@abstractmethod``
is not changed; all of them must be implemented by an explicit subclass
before it can be instantiated.


Merging and extending protocols
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The general philosophy is that protocols are mostly like regular ABCs,
but a static type checker will handle them specially. Subclassing a protocol
class would not turn the subclass into a protocol unless it also has
``typing.Protocol`` as an explicit base class. Without this base, the class
is "downgraded" to a regular ABC that cannot be used with structural
subtyping. The rationale for this rule is that we don't want to accidentally
have some class act as a protocol just because one of its base classes
happens to be one. We still slightly prefer nominal subtyping over structural
subtyping in the static typing world.

A subprotocol can be defined by having *both* one or more protocols as
immediate base classes and also having ``typing.Protocol`` as an immediate
base class::

  from typing import Protocol
  from collections.abc import Sized

  class SizedAndClosable(Sized, Protocol):
      def close(self) -> None:
          ...

Now the protocol ``SizedAndClosable`` is a protocol with two methods,
``__len__`` and ``close``. If one omits ``Protocol`` in the base class list,
this would be a regular (non-protocol) class that must implement ``Sized``.
Alternatively, one can implement ``SizedAndClosable`` protocol by merging
the ``SupportsClose`` protocol from the example in the `definition`_ section
with ``typing.Sized``::

  from collections.abc import Sized

  class SupportsClose(Protocol):
      def close(self) -> None:
          ...

  class SizedAndClosable(Sized, SupportsClose, Protocol):
      pass

The two definitions of ``SizedAndClosable`` are equivalent.
Subclass relationships between protocols are not meaningful when
considering subtyping, since structural compatibility is
the criterion, not the MRO.

If ``Protocol`` is included in the base class list, all the other base classes
must be protocols. A protocol can't extend a regular class.
Note that rules around explicit subclassing are different
from regular ABCs, where abstractness is simply defined by having at least one
abstract method being unimplemented. Protocol classes must be marked
*explicitly*.


Generic protocols
^^^^^^^^^^^^^^^^^

Generic protocols are important. For example, ``SupportsAbs``, ``Iterable``
and ``Iterator`` are generic protocols. They are defined similar to normal
non-protocol generic types::

  class Iterable(Protocol[T]):
      @abstractmethod
      def __iter__(self) -> Iterator[T]:
          ...

``Protocol[T, S, ...]`` is allowed as a shorthand for
``Protocol, Generic[T, S, ...]``.

User-defined generic protocols support explicitly declared variance.
Type checkers will warn if the inferred variance is different from
the declared variance. Examples::

  T = TypeVar('T')
  T_co = TypeVar('T_co', covariant=True)
  T_contra = TypeVar('T_contra', contravariant=True)

  class Box(Protocol[T_co]):
      def content(self) -> T_co:
          ...

  box: Box[float]
  second_box: Box[int]
  box = second_box  # This is OK due to the covariance of 'Box'.

  class Sender(Protocol[T_contra]):
      def send(self, data: T_contra) -> int:
          ...

  sender: Sender[float]
  new_sender: Sender[int]
  new_sender = sender  # OK, 'Sender' is contravariant.

  class Proto(Protocol[T]):
      attr: T  # this class is invariant, since it has a mutable attribute

  var: Proto[float]
  another_var: Proto[int]
  var = another_var  # Error! 'Proto[float]' is incompatible with 'Proto[int]'.

Note that unlike nominal classes, de facto covariant protocols cannot be
declared as invariant, since this can break transitivity of subtyping.
For example::

  T = TypeVar('T')

  class AnotherBox(Protocol[T]):  # Error, this protocol is covariant in T,
      def content(self) -> T:     # not invariant.
          ...


Recursive protocols
^^^^^^^^^^^^^^^^^^^

Recursive protocols are also supported. Forward references to the protocol
class names can be given as strings as specified by :pep:`484`. Recursive
protocols are useful for representing self-referential data structures
like trees in an abstract fashion::

  class Traversable(Protocol):
      def leaves(self) -> Iterable['Traversable']:
          ...

Note that for recursive protocols, a class is considered a subtype of
the protocol in situations where the decision depends on itself.
Continuing the previous example::

  class SimpleTree:
      def leaves(self) -> list['SimpleTree']:
          ...

  root: Traversable = SimpleTree()  # OK

  class Tree(Generic[T]):
      def leaves(self) -> list['Tree[T]']:
          ...

  def walk(graph: Traversable) -> None:
      ...
  tree: Tree[float] = Tree()
  walk(tree)  # OK, 'Tree[float]' is a subtype of 'Traversable'


Self-types in protocols
^^^^^^^^^^^^^^^^^^^^^^^

The self-types in protocols follow the
:pep:`corresponding specification <484#annotating-instance-and-class-methods>`
of :pep:`484`. For example::

  C = TypeVar('C', bound='Copyable')
  class Copyable(Protocol):
      def copy(self: C) -> C:

  class One:
      def copy(self) -> 'One':
          ...

  T = TypeVar('T', bound='Other')
  class Other:
      def copy(self: T) -> T:
          ...

  c: Copyable
  c = One()  # OK
  c = Other()  # Also OK


Callback protocols
^^^^^^^^^^^^^^^^^^

Protocols can be used to define flexible callback types that are hard
(or even impossible) to express using the ``Callable[...]`` syntax
specified by :pep:`484`, such as variadic, overloaded, and complex generic
callbacks. They can be defined as protocols with a ``__call__`` member::

  from typing import Protocol

  class Combiner(Protocol):
      def __call__(self, *vals: bytes,
                   maxlen: int | None = None) -> list[bytes]: ...

  def good_cb(*vals: bytes, maxlen: int | None = None) -> list[bytes]:
      ...
  def bad_cb(*vals: bytes, maxitems: int | None) -> list[bytes]:
      ...

  comb: Combiner = good_cb  # OK
  comb = bad_cb  # Error! Argument 2 has incompatible type because of
                 # different name and kind in the callback

Callback protocols and ``Callable[...]`` types can be used interchangeably.


Subtyping relationships with other types
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Protocols cannot be instantiated, so there are no values whose
runtime type is a protocol. For variables and parameters with protocol types,
subtyping relationships are subject to the following rules:

* A protocol is never a subtype of a concrete type.
* A concrete type ``X`` is a subtype of protocol ``P``
  if and only if ``X`` implements all protocol members of ``P`` with
  compatible types. In other words, subtyping with respect to a protocol is
  always structural.
* A protocol ``P1`` is a subtype of another protocol ``P2`` if ``P1`` defines
  all protocol members of ``P2`` with compatible types.

Generic protocol types follow the same rules of variance as non-protocol
types. Protocol types can be used in all contexts where any other types
can be used, such as in unions, ``ClassVar``, type variables bounds, etc.
Generic protocols follow the rules for generic abstract classes, except for
using structural compatibility instead of compatibility defined by
inheritance relationships.

Static type checkers will recognize protocol implementations, even if the
corresponding protocols are *not imported*::

  # file lib.py
  from collections.abc import Sized

  T = TypeVar('T', contravariant=True)
  class ListLike(Sized, Protocol[T]):
      def append(self, x: T) -> None:
          pass

  def populate(lst: ListLike[int]) -> None:
      ...

  # file main.py
  from lib import populate  # Note that ListLike is NOT imported

  class MockStack:
      def __len__(self) -> int:
          return 42
      def append(self, x: int) -> None:
          print(x)

  populate([1, 2, 3])    # Passes type check
  populate(MockStack())  # Also OK


Unions and intersections of protocols
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Unions of protocol classes behaves the same way as for non-protocol
classes. For example::

  from typing importt Protocol

  class Exitable(Protocol):
      def exit(self) -> int:
          ...
  class Quittable(Protocol):
      def quit(self) -> int | None:
          ...

  def finish(task: Exitable | Quittable) -> int:
      ...
  class DefaultJob:
      ...
      def quit(self) -> int:
          return 0
  finish(DefaultJob()) # OK

One can use multiple inheritance to define an intersection of protocols.
Example::

  from collections.abc import Iterable, Hashable

  class HashableFloats(Iterable[float], Hashable, Protocol):
      pass

  def cached_func(args: HashableFloats) -> float:
      ...
  cached_func((1, 2, 3)) # OK, tuple is both hashable and iterable


``Type[]`` and class objects vs protocols
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Variables and parameters annotated with ``Type[Proto]`` accept only concrete
(non-protocol) subtypes of ``Proto``. The main reason for this is to allow
instantiation of parameters with such types. For example::

  class Proto(Protocol):
      @abstractmethod
      def meth(self) -> int:
          ...
  class Concrete:
      def meth(self) -> int:
          return 42

  def fun(cls: Type[Proto]) -> int:
      return cls().meth() # OK
  fun(Proto)              # Error
  fun(Concrete)           # OK

The same rule applies to variables::

  var: Type[Proto]
  var = Proto    # Error
  var = Concrete # OK
  var().meth()   # OK

Assigning an ABC or a protocol class to a variable is allowed if it is
not explicitly typed, and such assignment creates a type alias.
For normal (non-abstract) classes, the behavior of ``Type[]`` is
not changed.

A class object is considered an implementation of a protocol if accessing
all members on it results in types compatible with the protocol members.
For example::

  from typing import Any, Protocol

  class ProtoA(Protocol):
      def meth(self, x: int) -> int: ...
  class ProtoB(Protocol):
      def meth(self, obj: Any, x: int) -> int: ...

  class C:
      def meth(self, x: int) -> int: ...

  a: ProtoA = C  # Type check error, signatures don't match!
  b: ProtoB = C  # OK


``NewType()`` and type aliases
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Protocols are essentially anonymous. To emphasize this point, static type
checkers might refuse protocol classes inside ``NewType()`` to avoid an
illusion that a distinct type is provided::

  from typing import NewType, Protocol
  from collections.abc import Iterator

  class Id(Protocol):
      code: int
      secrets: Iterator[bytes]

  UserId = NewType('UserId', Id)  # Error, can't provide distinct type

In contrast, type aliases are fully supported, including generic type
aliases::

  from typing import TypeVar
  from collections.abc import Reversible, Iterable, Sized

  T = TypeVar('T')
  class SizedIterable(Iterable[T], Sized, Protocol):
      pass
  CompatReversible = Reversible[T] | SizedIterable[T]


Modules as implementations of protocols
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

A module object is accepted where a protocol is expected if the public
interface of the given module is compatible with the expected protocol.
For example::

  # file default_config.py
  timeout = 100
  one_flag = True
  other_flag = False

  # file main.py
  import default_config
  from typing import Protocol

  class Options(Protocol):
      timeout: int
      one_flag: bool
      other_flag: bool

  def setup(options: Options) -> None:
      ...

  setup(default_config)  # OK

To determine compatibility of module level functions, the ``self`` argument
of the corresponding protocol methods is dropped. For example::

  # callbacks.py
  def on_error(x: int) -> None:
      ...
  def on_success() -> None:
      ...

  # main.py
  import callbacks
  from typing import Protocol

  class Reporter(Protocol):
      def on_error(self, x: int) -> None:
          ...
      def on_success(self) -> None:
          ...

  rp: Reporter = callbacks  # Passes type check


.. _discussion:

``@runtime_checkable`` decorator and narrowing types by ``isinstance()``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The default semantics is that ``isinstance()`` and ``issubclass()`` fail
for protocol types. This is in the spirit of duck typing -- protocols
basically would be used to model duck typing statically, not explicitly
at runtime.

However, it should be possible for protocol types to implement custom
instance and class checks when this makes sense, similar to how ``Iterable``
and other ABCs in ``collections.abc`` and ``typing`` already do it,
but this is limited to non-generic and unsubscripted generic protocols
(``Iterable`` is statically equivalent to ``Iterable[Any]``).
The ``typing`` module will define a special ``@runtime_checkable`` class decorator
that provides the same semantics for class and instance checks as for
``collections.abc`` classes, essentially making them "runtime protocols"::

  from typing import runtime_checkable, Protocol

  @runtime_checkable
  class SupportsClose(Protocol):
      def close(self):
          ...

  assert isinstance(open('some/file'), SupportsClose)

Note that instance checks are not 100% reliable statically, which is why
this behavior is opt-in.
The most type checkers can do is to treat ``isinstance(obj, Iterator)``
roughly as a simpler way to write
``hasattr(x, '__iter__') and hasattr(x, '__next__')``. To minimize
the risks for this feature, the following rules are applied.

**Definitions**:

* *Data and non-data protocols*: A protocol is called a non-data protocol
  if it only contains methods as members (for example ``Sized``,
  ``Iterator``, etc). A protocol that contains at least one non-method member
  (like ``x: int``) is called a data protocol.
* *Unsafe overlap*: A type ``X`` is called unsafely overlapping with
  a protocol ``P``, if ``X`` is not a subtype of ``P``, but it is a subtype
  of the type erased version of ``P`` where all members have type ``Any``.
  In addition, if at least one element of a union unsafely overlaps with
  a protocol ``P``, then the whole union is unsafely overlapping with ``P``.

**Specification**:

* A protocol can be used as a second argument in ``isinstance()`` and
  ``issubclass()`` only if it is explicitly opt-in by ``@runtime_checkable``
  decorator. This requirement exists because protocol checks are not type safe
  in case of dynamically set attributes, and because type checkers can only prove
  that an ``isinstance()`` check is safe only for a given class, not for all its
  subclasses.
* ``isinstance()`` can be used with both data and non-data protocols, while
  ``issubclass()`` can be used only with non-data protocols. This restriction
  exists because some data attributes can be set on an instance in constructor
  and this information is not always available on the class object.
* Type checkers should reject an ``isinstance()`` or ``issubclass()`` call, if
  there is an unsafe overlap between the type of the first argument and
  the protocol.
* Type checkers should be able to select a correct element from a union after
  a safe ``isinstance()`` or ``issubclass()`` call. For narrowing from non-union
  types, type checkers can use their best judgement (this is intentionally
  unspecified, since a precise specification would require intersection types).


Function/method overloading
---------------------------

The ``@overload`` decorator allows describing functions and methods
that support multiple different combinations of argument types.  This
pattern is used frequently in builtin modules and types.  For example,
the ``__getitem__()`` method of the ``bytes`` type can be described as
follows::

  from typing import overload

  class bytes:
      ...
      @overload
      def __getitem__(self, i: int) -> int: ...
      @overload
      def __getitem__(self, s: slice) -> bytes: ...

This description is more precise than would be possible using unions
(which cannot express the relationship between the argument and return
types)::

  class bytes:
      ...
      def __getitem__(self, a: int | slice) -> int | bytes: ...

Another example where ``@overload`` comes in handy is the type of the
builtin ``map()`` function, which takes a different number of
arguments depending on the type of the callable::

  from typing import TypeVar, overload
  from collections.abc import Callable, Iterable, Iterator

  T1 = TypeVar('T1')
  T2 = TypeVar('T2')
  S = TypeVar('S')

  @overload
  def map(func: Callable[[T1], S], iter1: Iterable[T1]) -> Iterator[S]: ...
  @overload
  def map(func: Callable[[T1, T2], S],
          iter1: Iterable[T1], iter2: Iterable[T2]) -> Iterator[S]: ...
  # ... and we could add more items to support more than two iterables

Note that we could also easily add items to support ``map(None, ...)``::

  @overload
  def map(func: None, iter1: Iterable[T1]) -> Iterable[T1]: ...
  @overload
  def map(func: None,
          iter1: Iterable[T1],
          iter2: Iterable[T2]) -> Iterable[tuple[T1, T2]]: ...

Uses of the ``@overload`` decorator as shown above are suitable for
stub files.  In regular modules, a series of ``@overload``-decorated
definitions must be followed by exactly one
non-``@overload``-decorated definition (for the same function/method).
The ``@overload``-decorated definitions are for the benefit of the
type checker only, since they will be overwritten by the
non-``@overload``-decorated definition, while the latter is used at
runtime but should be ignored by a type checker.  At runtime, calling
a ``@overload``-decorated function directly will raise
``NotImplementedError``.  Here's an example of a non-stub overload
that can't easily be expressed using a union or a type variable::

  @overload
  def utf8(value: None) -> None:
      pass
  @overload
  def utf8(value: bytes) -> bytes:
      pass
  @overload
  def utf8(value: unicode) -> bytes:
      pass
  def utf8(value):
      <actual implementation>

NOTE: While it would be possible to provide a multiple dispatch
implementation using this syntax, its implementation would require
using ``sys._getframe()``, which is frowned upon.  Also, designing and
implementing an efficient multiple dispatch mechanism is hard, which
is why previous attempts were abandoned in favor of
``functools.singledispatch()``.  (See :pep:`443`, especially its section
"Alternative approaches".)  In the future we may come up with a
satisfactory multiple dispatch design, but we don't want such a design
to be constrained by the overloading syntax defined for type hints in
stub files.  It is also possible that both features will develop
independent from each other (since overloading in the type checker
has different use cases and requirements than multiple dispatch
at runtime -- e.g. the latter is unlikely to support generic types).

A constrained ``TypeVar`` type can often be used instead of using the
``@overload`` decorator.  For example, the definitions of ``concat1``
and ``concat2`` in this stub file are equivalent::

  from typing import TypeVar

  AnyStr = TypeVar('AnyStr', str, bytes)

  def concat1(x: AnyStr, y: AnyStr) -> AnyStr: ...

  @overload
  def concat2(x: str, y: str) -> str: ...
  @overload
  def concat2(x: bytes, y: bytes) -> bytes: ...

Some functions, such as ``map`` or ``bytes.__getitem__`` above, can't
be represented precisely using type variables. We
recommend that ``@overload`` is only used in cases where a type
variable is not sufficient.

Another important difference between type variables such as ``AnyStr``
and using ``@overload`` is that the prior can also be used to define
constraints for generic class type parameters.  For example, the type
parameter of the generic class ``typing.IO`` is constrained (only
``IO[str]``, ``IO[bytes]`` and ``IO[Any]`` are valid)::

  class IO(Generic[AnyStr]): ...

Literal
-------

(Originally specified in :pep:`586`.)


Core Semantics
^^^^^^^^^^^^^^

This section outlines the baseline behavior of literal types.

Core behavior
"""""""""""""

Literal types indicate that a variable has a specific and
concrete value. For example, if we define some variable ``foo`` to have
type ``Literal[3]``, we are declaring that ``foo`` must be exactly equal
to ``3`` and no other value.

Given some value ``v`` that is a member of type ``T``, the type
``Literal[v]`` shall be treated as a subtype of ``T``. For example,
``Literal[3]`` is a subtype of ``int``.

All methods from the parent type will be directly inherited by the
literal type. So, if we have some variable ``foo`` of type ``Literal[3]``
it’s safe to do things like ``foo + 5`` since ``foo`` inherits ``int``’s
``__add__`` method. The resulting type of ``foo + 5`` is ``int``.

This "inheriting" behavior is identical to how we
:pep:`handle NewTypes <484#newtype-helper-function>`.

Equivalence of two Literals
"""""""""""""""""""""""""""

Two types ``Literal[v1]`` and ``Literal[v2]`` are equivalent when
both of the following conditions are true:

1. ``type(v1) == type(v2)``
2. ``v1 == v2``

For example, ``Literal[20]`` and ``Literal[0x14]`` are equivalent.
However, ``Literal[0]`` and ``Literal[False]`` are *not* equivalent
despite that ``0 == False`` evaluates to 'true' at runtime: ``0``
has type ``int`` and ``False`` has type ``bool``.

Shortening unions of literals
"""""""""""""""""""""""""""""

Literals are parameterized with one or more values. When a Literal is
parameterized with more than one value, it's treated as exactly equivalent
to the union of those types. That is, ``Literal[v1, v2, v3]`` is equivalent
to ``Literal[v1] | Literal[v2] | Literal[v3]``.

This shortcut helps make writing signatures for functions that accept
many different literals more ergonomic — for example, functions like
``open(...)``::

   # Note: this is a simplification of the true type signature.
   _PathType = str | bytes | int

   @overload
   def open(path: _PathType,
            mode: Literal["r", "w", "a", "x", "r+", "w+", "a+", "x+"],
            ) -> IO[str]: ...
   @overload
   def open(path: _PathType,
            mode: Literal["rb", "wb", "ab", "xb", "r+b", "w+b", "a+b", "x+b"],
            ) -> IO[bytes]: ...

   # Fallback overload for when the user isn't using literal types
   @overload
   def open(path: _PathType, mode: str) -> IO[Any]: ...

The provided values do not all have to be members of the same type.
For example, ``Literal[42, "foo", True]`` is a legal type.

However, Literal **must** be parameterized with at least one type.
Types like ``Literal[]`` or ``Literal`` are illegal.


Legal and illegal parameterizations
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This section describes what exactly constitutes a legal ``Literal[...]`` type:
what values may and may not be used as parameters.

In short, a ``Literal[...]`` type may be parameterized by one or more literal
expressions, and nothing else.


Legal parameters for ``Literal`` at type check time
"""""""""""""""""""""""""""""""""""""""""""""""""""

``Literal`` may be parameterized with literal ints, byte and unicode strings,
bools, Enum values and ``None``. So for example, all of
the following would be legal::

   Literal[26]
   Literal[0x1A]  # Exactly equivalent to Literal[26]
   Literal[-4]
   Literal["hello world"]
   Literal[b"hello world"]
   Literal[u"hello world"]
   Literal[True]
   Literal[Color.RED]  # Assuming Color is some enum
   Literal[None]

**Note:** Since the type ``None`` is inhabited by just a single
value, the types ``None`` and ``Literal[None]`` are exactly equivalent.
Type checkers may simplify ``Literal[None]`` into just ``None``.

``Literal`` may also be parameterized by other literal types, or type aliases
to other literal types. For example, the following is legal::

    ReadOnlyMode         = Literal["r", "r+"]
    WriteAndTruncateMode = Literal["w", "w+", "wt", "w+t"]
    WriteNoTruncateMode  = Literal["r+", "r+t"]
    AppendMode           = Literal["a", "a+", "at", "a+t"]

    AllModes = Literal[ReadOnlyMode, WriteAndTruncateMode,
                       WriteNoTruncateMode, AppendMode]

This feature is again intended to help make using and reusing literal types
more ergonomic.

**Note:** As a consequence of the above rules, type checkers are also expected
to support types that look like the following::

    Literal[Literal[Literal[1, 2, 3], "foo"], 5, None]

This should be exactly equivalent to the following type::

    Literal[1, 2, 3, "foo", 5, None]

...and also to the following type::

    Literal[1, 2, 3, "foo", 5] | None

**Note:** String literal types like ``Literal["foo"]`` should subtype either
bytes or unicode in the same way regular string literals do at runtime.

For example, in Python 3, the type ``Literal["foo"]`` is equivalent to
``Literal[u"foo"]``, since ``"foo"`` is equivalent to ``u"foo"`` in Python 3.

Similarly, in Python 2, the type ``Literal["foo"]`` is equivalent to
``Literal[b"foo"]`` -- unless the file includes a
``from __future__ import unicode_literals`` import, in which case it would be
equivalent to ``Literal[u"foo"]``.

Illegal parameters for ``Literal`` at type check time
"""""""""""""""""""""""""""""""""""""""""""""""""""""

The following parameters are intentionally disallowed by design:

- Arbitrary expressions like ``Literal[3 + 4]`` or
  ``Literal["foo".replace("o", "b")]``.

  - Rationale: Literal types are meant to be a
    minimal extension to the :pep:`484` typing ecosystem and requiring type
    checkers to interpret potentially expressions inside types adds too
    much complexity.

  - As a consequence, complex numbers like ``Literal[4 + 3j]`` and
    ``Literal[-4 + 2j]`` are also prohibited. For consistency, literals like
    ``Literal[4j]`` that contain just a single complex number are also
    prohibited.

  - The only exception to this rule is the unary ``-`` (minus) for ints: types
    like ``Literal[-5]`` are *accepted*.

-  Tuples containing valid literal types like ``Literal[(1, "foo", "bar")]``.
   The user could always express this type as
   ``tuple[Literal[1], Literal["foo"], Literal["bar"]]`` instead. Also,
   tuples are likely to be confused with the ``Literal[1, 2, 3]``
   shortcut.

-  Mutable literal data structures like dict literals, list literals, or
   set literals: literals are always implicitly final and immutable. So,
   ``Literal[{"a": "b", "c": "d"}]`` is illegal.

-  Any other types: for example, ``Literal[Path]``, or
   ``Literal[some_object_instance]`` are illegal. This includes typevars: if
   ``T`` is a typevar,  ``Literal[T]`` is not allowed. Typevars can vary over
   only types, never over values.

The following are provisionally disallowed for simplicity. We can consider
allowing them in the future.

-  Floats: e.g. ``Literal[3.14]``. Representing Literals of infinity or NaN
   in a clean way is tricky; real-world APIs are unlikely to vary their
   behavior based on a float parameter.
  
-  Any: e.g. ``Literal[Any]``. ``Any`` is a type, and ``Literal[...]`` is
   meant to contain values only. It is also unclear what ``Literal[Any]``
   would actually semantically mean.

Parameters at runtime
"""""""""""""""""""""

Although the set of parameters ``Literal[...]`` may contain at type check time
is very small, the actual implementation of ``typing.Literal`` will not perform
any checks at runtime. For example::

   def my_function(x: Literal[1 + 2]) -> int:
       return x * 3

   x: Literal = 3
   y: Literal[my_function] = my_function

The type checker should reject this program: all three uses of
``Literal`` are *invalid* according to this spec. However, Python itself
should execute this program with no errors.

This is partly to help us preserve flexibility in case we want to expand the
scope of what ``Literal`` can be used for in the future, and partly because
it is not possible to detect all illegal parameters at runtime to begin with.
For example, it is impossible to distinguish between ``Literal[1 + 2]`` and
``Literal[3]`` at runtime.

Literals, enums, and forward references
"""""""""""""""""""""""""""""""""""""""

One potential ambiguity is between literal strings and forward
references to literal enum members. For example, suppose we have the
type ``Literal["Color.RED"]``. Does this literal type
contain a string literal or a forward reference to some ``Color.RED``
enum member?

In cases like these, we always assume the user meant to construct a
literal string. If the user wants a forward reference, they must wrap
the entire literal type in a string -- e.g. ``"Literal[Color.RED]"``.

Type inference
^^^^^^^^^^^^^^

This section describes a few rules regarding type inference and
literals, along with some examples.

Backwards compatibility
"""""""""""""""""""""""

When type checkers add support for Literal, it's important they do so
in a way that maximizes backwards-compatibility. Type checkers should
ensure that code that used to type check continues to do so after support
for Literal is added on a best-effort basis.

This is particularly important when performing type inference. For
example, given the statement ``x = "blue"``, should the inferred
type of ``x`` be ``str`` or ``Literal["blue"]``?

One naive strategy would be to always assume expressions are intended
to be Literal types. So, ``x`` would always have an inferred type of
``Literal["blue"]`` in the example above. This naive strategy is almost
certainly too disruptive -- it would cause programs like the following
to start failing when they previously did not::

    # If a type checker infers 'var' has type Literal[3]
    # and my_list has type List[Literal[3]]...
    var = 3
    my_list = [var]

    # ...this call would be a type-error.
    my_list.append(4)

Another example of when this strategy would fail is when setting fields
in objects::

    class MyObject:
        def __init__(self) -> None:
            # If a type checker infers MyObject.field has type Literal[3]...
            self.field = 3

    m = MyObject()

    # ...this assignment would no longer type check
    m.field = 4

An alternative strategy that *does* maintain compatibility in every case would
be to always assume expressions are *not* Literal types unless they are
explicitly annotated otherwise. A type checker using this strategy would
always infer that ``x`` is of type ``str`` in the first example above.

This is not the only viable strategy: type checkers should feel free to experiment
with more sophisticated inference techniques. No particular strategy is
mandated, but type checkers should keep in mind the importance of backwards
compatibility.

Using non-Literals in Literal contexts
""""""""""""""""""""""""""""""""""""""

Literal types follow the existing rules regarding subtyping with no additional
special-casing. For example, programs like the following are type safe::

   def expects_str(x: str) -> None: ...
   var: Literal["foo"] = "foo"

   # Legal: Literal["foo"] is a subtype of str
   expects_str(var)

This also means non-Literal expressions in general should not automatically
be cast to Literal. For example::

   def expects_literal(x: Literal["foo"]) -> None: ...

   def runner(my_str: str) -> None:
       # ILLEGAL: str is not a subclass of Literal["foo"]
       expects_literal(my_str)

**Note:** If the user wants their API to support accepting both literals
*and* the original type -- perhaps for legacy purposes -- they should
implement a fallback overload. See `Interactions with overloads`_.

Interactions with other types and features
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This section discusses how Literal types interact with other existing types.

Intelligent indexing of structured data
"""""""""""""""""""""""""""""""""""""""

Literals can be used to "intelligently index" into structured types like
tuples, NamedTuple, and classes. (Note: this is not an exhaustive list).

For example, type checkers should infer the correct value type when
indexing into a tuple using an int key that corresponds a valid index::

   a: Literal[0] = 0
   b: Literal[5] = 5

   some_tuple: tuple[int, str, List[bool]] = (3, "abc", [True, False])
   reveal_type(some_tuple[a])   # Revealed type is 'int'
   some_tuple[b]                # Error: 5 is not a valid index into the tuple

We expect similar behavior when using functions like getattr::

   class Test:
       def __init__(self, param: int) -> None:
           self.myfield = param

       def mymethod(self, val: int) -> str: ...

   a: Literal["myfield"]  = "myfield"
   b: Literal["mymethod"] = "mymethod"
   c: Literal["blah"]     = "blah"

   t = Test()
   reveal_type(getattr(t, a))  # Revealed type is 'int'
   reveal_type(getattr(t, b))  # Revealed type is 'Callable[[int], str]'
   getattr(t, c)               # Error: No attribute named 'blah' in Test

**Note:** See `Interactions with Final`_ for how we can
express the variable declarations above in a more compact manner.

Interactions with overloads
"""""""""""""""""""""""""""

Literal types and overloads do not need to interact in  a special
way: the existing rules work fine.

However, one important use case type checkers must take care to
support is the ability to use a *fallback* when the user is not using literal
types. For example, consider ``open``::

   _PathType = str | bytes | int

   @overload
   def open(path: _PathType,
            mode: Literal["r", "w", "a", "x", "r+", "w+", "a+", "x+"],
            ) -> IO[str]: ...
   @overload
   def open(path: _PathType,
            mode: Literal["rb", "wb", "ab", "xb", "r+b", "w+b", "a+b", "x+b"],
            ) -> IO[bytes]: ...

   # Fallback overload for when the user isn't using literal types
   @overload
   def open(path: _PathType, mode: str) -> IO[Any]: ...

If we were to change the signature of ``open`` to use just the first two overloads,
we would break any code that does not pass in a literal string expression.
For example, code like this would be broken::

   mode: str = pick_file_mode(...)
   with open(path, mode) as f:
       # f should continue to be of type IO[Any] here

A little more broadly: we mandate that whenever we add literal types to
some existing API in typeshed, we also always include a fallback overload to
maintain backwards-compatibility.

Interactions with generics
""""""""""""""""""""""""""

Types like ``Literal[3]`` are meant to be just plain old subclasses of
``int``. This means you can use types like ``Literal[3]`` anywhere
you could use normal types, such as with generics.

This means that it is legal to parameterize generic functions or
classes using Literal types::

   A = TypeVar('A', bound=int)
   B = TypeVar('B', bound=int)
   C = TypeVar('C', bound=int)

   # A simplified definition for Matrix[row, column]
   class Matrix(Generic[A, B]):
       def __add__(self, other: Matrix[A, B]) -> Matrix[A, B]: ...
       def __matmul__(self, other: Matrix[B, C]) -> Matrix[A, C]: ...
       def transpose(self) -> Matrix[B, A]: ...

   foo: Matrix[Literal[2], Literal[3]] = Matrix(...)
   bar: Matrix[Literal[3], Literal[7]] = Matrix(...)

   baz = foo @ bar
   reveal_type(baz)  # Revealed type is 'Matrix[Literal[2], Literal[7]]'

Similarly, it is legal to construct TypeVars with value restrictions
or bounds involving Literal types::

   T = TypeVar('T', Literal["a"], Literal["b"], Literal["c"])
   S = TypeVar('S', bound=Literal["foo"])

...although it is unclear when it would ever be useful to construct a
TypeVar with a Literal upper bound. For example, the ``S`` TypeVar in
the above example is essentially pointless: we can get equivalent behavior
by using ``S = Literal["foo"]`` instead.

**Note:** Literal types and generics deliberately interact in only very
basic and limited ways. In particular, libraries that want to type check
code containing a heavy amount of numeric or numpy-style manipulation will
almost certainly likely find Literal types as described here to be
insufficient for their needs.

Interactions with enums and exhaustiveness checks
"""""""""""""""""""""""""""""""""""""""""""""""""

Type checkers should be capable of performing exhaustiveness checks when
working Literal types that have a closed number of variants, such as
enums. For example, the type checker should be capable of inferring that
the final ``else`` statement must be of type ``str``, since all three
values of the ``Status`` enum have already been exhausted::

    class Status(Enum):
        SUCCESS = 0
        INVALID_DATA = 1
        FATAL_ERROR = 2

    def parse_status(s: str | Status) -> None:
        if s is Status.SUCCESS:
            print("Success!")
        elif s is Status.INVALID_DATA:
            print("The given data is invalid because...")
        elif s is Status.FATAL_ERROR:
            print("Unexpected fatal error...")
        else:
            # 's' must be of type 'str' since all other options are exhausted
            print("Got custom status: " + s)

The interaction described above is not new: it's already
:pep:`codified within PEP 484 <484#support-for-singleton-types-in-unions>`.
However, many type
checkers (such as mypy) do not yet implement this due to the expected
complexity of the implementation work.

Some of this complexity will be alleviated once Literal types are introduced:
rather than entirely special-casing enums, we can instead treat them as being
approximately equivalent to the union of their values and take advantage of any
existing logic regarding unions, exhaustibility, type narrowing, reachability,
and so forth the type checker might have already implemented.

So here, the ``Status`` enum could be treated as being approximately equivalent
to ``Literal[Status.SUCCESS, Status.INVALID_DATA, Status.FATAL_ERROR]``
and the type of ``s`` narrowed accordingly.

Interactions with narrowing
"""""""""""""""""""""""""""

Type checkers may optionally perform additional analysis for both enum and
non-enum Literal types beyond what is described in the section above.

For example, it may be useful to perform narrowing based on things like
containment or equality checks::

   def parse_status(status: str) -> None:
       if status in ("MALFORMED", "ABORTED"):
           # Type checker could narrow 'status' to type
           # Literal["MALFORMED", "ABORTED"] here.
           return expects_bad_status(status)

       # Similarly, type checker could narrow 'status' to Literal["PENDING"]
       if status == "PENDING":
           expects_pending_status(status)

It may also be useful to perform narrowing taking into account expressions
involving Literal bools. For example, we can combine ``Literal[True]``,
``Literal[False]``, and overloads to construct "custom type guards"::

   @overload
   def is_int_like(x: int | list[int]) -> Literal[True]: ...
   @overload
   def is_int_like(x: object) -> bool: ...
   def is_int_like(x): ...

   vector: list[int] = [1, 2, 3]
   if is_int_like(vector):
       vector.append(3)
   else:
       vector.append("bad")   # This branch is inferred to be unreachable

   scalar: int | str
   if is_int_like(scalar):
       scalar += 3      # Type checks: type of 'scalar' is narrowed to 'int'
   else:
       scalar += "foo"  # Type checks: type of 'scalar' is narrowed to 'str'
    
Interactions with Final
"""""""""""""""""""""""

The ``Final`` qualifier can be used to declare that some variable or
attribute cannot be reassigned::

    foo: Final = 3
    foo = 4           # Error: 'foo' is declared to be Final

Note that in the example above, we know that ``foo`` will always be equal to
exactly ``3``. A type checker can use this information to deduce that ``foo``
is valid to use in any context that expects a ``Literal[3]``::

    def expects_three(x: Literal[3]) -> None: ...

    expects_three(foo)  # Type checks, since 'foo' is Final and equal to 3

The ``Final`` qualifier serves as a shorthand for declaring that a variable
is *effectively Literal*.

Type checkers are expected to
support this shortcut. Specifically, given a variable or attribute assignment
of the form ``var: Final = value`` where ``value`` is a valid parameter for
``Literal[...]``, type checkers should understand that ``var`` may be used in
any context that expects a ``Literal[value]``.

Type checkers are not obligated to understand any other uses of Final. For
example, whether or not the following program type checks is left unspecified::

    # Note: The assignment does not exactly match the form 'var: Final = value'.
    bar1: Final[int] = 3
    expects_three(bar1)  # May or may not be accepted by type checkers

    # Note: "Literal[1 + 2]" is not a legal type.
    bar2: Final = 1 + 2
    expects_three(bar2)  # May or may not be accepted by type checkers


TypedDict
---------

(Originally specified in :pep:`589`.)

A TypedDict type represents dictionary objects with a specific set of
string keys, and with specific value types for each valid key.  Each
string key can be either required (it must be present) or
non-required (it doesn't need to exist).

There are two ways of defining TypedDict types.  The first uses
a class-based syntax.  The second is an alternative
assignment-based syntax that is provided for backwards compatibility,
to allow the feature to be backported to older Python versions.  The
rationale is similar to why :pep:`484` supports a comment-based
annotation syntax for Python 2.7: type hinting is particularly useful
for large existing codebases, and these often need to run on older
Python versions.  The two syntax options parallel the syntax variants
supported by ``typing.NamedTuple``.  Other features include
TypedDict inheritance and totality (specifying whether keys are
required or not).

This section also provides a sketch of how a type checker is expected
to support type checking operations involving TypedDict objects.
Similar to :pep:`484`, this discussion is left somewhat vague on purpose,
to allow experimentation with a wide variety of different type
checking approaches.  In particular, type compatibility should be
based on structural compatibility: a more specific TypedDict type can
be compatible with a smaller (more general) TypedDict type.


Class-based Syntax
^^^^^^^^^^^^^^^^^^

A TypedDict type can be defined using the class definition syntax with
``typing.TypedDict`` as the sole base class::

    from typing import TypedDict

    class Movie(TypedDict):
        name: str
        year: int

``Movie`` is a TypedDict type with two items: ``'name'`` (with type
``str``) and ``'year'`` (with type ``int``).

A type checker should validate that the body of a class-based
TypedDict definition conforms to the following rules:

* The class body should only contain lines with item definitions of the
  form ``key: value_type``, optionally preceded by a docstring.  The
  syntax for item definitions is identical to attribute annotations,
  but there must be no initializer, and the key name actually refers
  to the string value of the key instead of an attribute name.

* Type comments cannot be used with the class-based syntax, for
  consistency with the class-based ``NamedTuple`` syntax.  (Note that
  it would not be sufficient to support type comments for backwards
  compatibility with Python 2.7, since the class definition may have a
  ``total`` keyword argument, as discussed below, and this isn't valid
  syntax in Python 2.7.)  Instead, `Alternative Syntax`_ provides an
  alternative, assignment-based syntax for backwards compatibility.

* String literal forward references are valid in the value types.

* Methods are not allowed, since the runtime type of a TypedDict
  object will always be just ``dict`` (it is never a subclass of
  ``dict``).

* Specifying a metaclass is not allowed.

* TypedDicts may be made generic by adding ``Generic[T]`` among the
  bases (or, in Python 3.12 and higher, by using the new
  syntax for generic classes).

An empty TypedDict can be created by only including ``pass`` in the
body (if there is a docstring, ``pass`` can be omitted)::

    class EmptyDict(TypedDict):
        pass


Using TypedDict Types
^^^^^^^^^^^^^^^^^^^^^

Here is an example of how the type ``Movie`` can be used::

    movie: Movie = {'name': 'Blade Runner',
                    'year': 1982}

An explicit ``Movie`` type annotation is generally needed, as
otherwise an ordinary dictionary type could be assumed by a type
checker, for backwards compatibility.  When a type checker can infer
that a constructed dictionary object should be a TypedDict, an
explicit annotation can be omitted.  A typical example is a dictionary
object as a function argument.  In this example, a type checker is
expected to infer that the dictionary argument should be understood as
a TypedDict::

    def record_movie(movie: Movie) -> None: ...

    record_movie({'name': 'Blade Runner', 'year': 1982})

Another example where a type checker should treat a dictionary display
as a TypedDict is in an assignment to a variable with a previously
declared TypedDict type::

    movie: Movie
    ...
    movie = {'name': 'Blade Runner', 'year': 1982}

Operations on ``movie`` can be checked by a static type checker::

    movie['director'] = 'Ridley Scott'  # Error: invalid key 'director'
    movie['year'] = '1982'  # Error: invalid value type ("int" expected)

The code below should be rejected, since ``'title'`` is not a valid
key, and the ``'name'`` key is missing::

    movie2: Movie = {'title': 'Blade Runner',
                     'year': 1982}

The created TypedDict type object is not a real class object.  Here
are the only uses of the type a type checker is expected to allow:

* It can be used in type annotations and in any context where an
  arbitrary type hint is valid, such as in type aliases and as the
  target type of a cast.

* It can be used as a callable object with keyword arguments
  corresponding to the TypedDict items.  Non-keyword arguments are not
  allowed.  Example::

      m = Movie(name='Blade Runner', year=1982)

  When called, the TypedDict type object returns an ordinary
  dictionary object at runtime::

      print(type(m))  # <class 'dict'>

* It can be used as a base class, but only when defining a derived
  TypedDict.  This is discussed in more detail below.

In particular, TypedDict type objects cannot be used in
``isinstance()`` tests such as ``isinstance(d, Movie)``. The reason is
that there is no existing support for checking types of dictionary
item values, since ``isinstance()`` does not work with many :pep:`484`
types, including common ones like ``List[str]``.  This would be needed
for cases like this::

    class Strings(TypedDict):
        items: List[str]

    print(isinstance({'items': [1]}, Strings))    # Should be False
    print(isinstance({'items': ['x']}, Strings))  # Should be True

The above use case is not supported.  This is consistent with how
``isinstance()`` is not supported for ``List[str]``.


Inheritance
^^^^^^^^^^^

It is possible for a TypedDict type to inherit from one or more
TypedDict types using the class-based syntax.  In this case the
``TypedDict`` base class should not be included.  Example::

    class BookBasedMovie(Movie):
        based_on: str

Now ``BookBasedMovie`` has keys ``name``, ``year``, and ``based_on``.
It is equivalent to this definition, since TypedDict types use
structural compatibility::

    class BookBasedMovie(TypedDict):
        name: str
        year: int
        based_on: str

Here is an example of multiple inheritance::

    class X(TypedDict):
        x: int

    class Y(TypedDict):
        y: str

    class XYZ(X, Y):
        z: bool

The TypedDict ``XYZ`` has three items: ``x`` (type ``int``), ``y``
(type ``str``), and ``z`` (type ``bool``).

A TypedDict cannot inherit from both a TypedDict type and a
non-TypedDict base class other than ``Generic``.

Additional notes on TypedDict class inheritance:

* Changing a field type of a parent TypedDict class in a subclass is not allowed.
  Example::

   class X(TypedDict):
      x: str

   class Y(X):
      x: int  # Type check error: cannot overwrite TypedDict field "x"

  In the example outlined above TypedDict class annotations returns
  type ``str`` for key ``x``::

   print(Y.__annotations__)  # {'x': <class 'str'>}


* Multiple inheritance does not allow conflict types for the same name field::

   class X(TypedDict):
      x: int

   class Y(TypedDict):
      x: str

   class XYZ(X, Y):  # Type check error: cannot overwrite TypedDict field "x" while merging
      xyz: bool


Totality
^^^^^^^^

By default, all keys must be present in a TypedDict.  It is possible
to override this by specifying *totality*.  Here is how to do this
using the class-based syntax::

    class Movie(TypedDict, total=False):
        name: str
        year: int

This means that a ``Movie`` TypedDict can have any of the keys omitted. Thus
these are valid::

    m: Movie = {}
    m2: Movie = {'year': 2015}

A type checker is only expected to support a literal ``False`` or
``True`` as the value of the ``total`` argument.  ``True`` is the
default, and makes all items defined in the class body be required.

The totality flag only applies to items defined in the body of the
TypedDict definition.  Inherited items won't be affected, and instead
use totality of the TypedDict type where they were defined.  This makes
it possible to have a combination of required and non-required keys in
a single TypedDict type. Alternatively, ``Required`` and ``NotRequired``
(see below) can be used to mark individual items as required or non-required.


Alternative Syntax
^^^^^^^^^^^^^^^^^^

This section provides an alternative syntax that can be backported to
older Python versions such as 3.5 and 2.7 that don't support the
variable definition syntax introduced in :pep:`526`.  It
resembles the traditional syntax for defining named tuples::

    Movie = TypedDict('Movie', {'name': str, 'year': int})

It is also possible to specify totality using the alternative syntax::

    Movie = TypedDict('Movie',
                      {'name': str, 'year': int},
                      total=False)

The semantics are equivalent to the class-based syntax.  This syntax
doesn't support inheritance, however.  The
motivation for this is keeping the backwards compatible syntax as
simple as possible while covering the most common use cases.

A type checker is only expected to accept a dictionary display expression
as the second argument to ``TypedDict``.  In particular, a variable that
refers to a dictionary object does not need to be supported, to simplify
implementation.


Type Consistency
^^^^^^^^^^^^^^^^

Informally speaking, *type consistency* is a generalization of the
is-subtype-of relation to support the ``Any`` type.  It is defined
more formally in :pep:`483`.  This section introduces the
new, non-trivial rules needed to support type consistency for
TypedDict types.

First, any TypedDict type is consistent with ``Mapping[str, object]``.
Second, a TypedDict type ``A`` is consistent with TypedDict ``B`` if
``A`` is structurally compatible with ``B``.  This is true if and only
if both of these conditions are satisfied:

* For each key in ``B``, ``A`` has the corresponding key and the
  corresponding value type in ``A`` is consistent with the value type
  in ``B``.  For each key in ``B``, the value type in ``B`` is also
  consistent with the corresponding value type in ``A``.

* For each required key in ``B``, the corresponding key is required
  in ``A``.  For each non-required key in ``B``, the corresponding key
  is not required in ``A``.

Discussion:

* Value types behave invariantly, since TypedDict objects are mutable.
  This is similar to mutable container types such as ``List`` and
  ``Dict``.  Example where this is relevant::

      class A(TypedDict):
          x: int | None

      class B(TypedDict):
          x: int

      def f(a: A) -> None:
          a['x'] = None

      b: B = {'x': 0}
      f(b)  # Type check error: 'B' not compatible with 'A'
      b['x'] + 1  # Runtime error: None + 1

* A TypedDict type with a required key is not consistent with a
  TypedDict type where the same key is a non-required key, since the
  latter allows keys to be deleted.  Example where this is relevant::

      class A(TypedDict, total=False):
          x: int

      class B(TypedDict):
          x: int

      def f(a: A) -> None:
          del a['x']

      b: B = {'x': 0}
      f(b)  # Type check error: 'B' not compatible with 'A'
      b['x'] + 1  # Runtime KeyError: 'x'

* A TypedDict type ``A`` with no key ``'x'`` is not consistent with a
  TypedDict type with a non-required key ``'x'``, since at runtime
  the key ``'x'`` could be present and have an incompatible type
  (which may not be visible through ``A`` due to structural subtyping).
  Example::

      class A(TypedDict, total=False):
          x: int
          y: int

      class B(TypedDict, total=False):
          x: int

      class C(TypedDict, total=False):
          x: int
          y: str

       def f(a: A) -> None:
           a['y'] = 1

       def g(b: B) -> None:
           f(b)  # Type check error: 'B' incompatible with 'A'

       c: C = {'x': 0, 'y': 'foo'}
       g(c)
       c['y'] + 'bar'  # Runtime error: int + str

* A TypedDict isn't consistent with any ``Dict[...]`` type, since
  dictionary types allow destructive operations, including
  ``clear()``.  They also allow arbitrary keys to be set, which
  would compromise type safety.  Example::

      class A(TypedDict):
          x: int

      class B(A):
          y: str

      def f(d: Dict[str, int]) -> None:
          d['y'] = 0

      def g(a: A) -> None:
          f(a)  # Type check error: 'A' incompatible with Dict[str, int]

      b: B = {'x': 0, 'y': 'foo'}
      g(b)
      b['y'] + 'bar'  # Runtime error: int + str

* A TypedDict with all ``int`` values is not consistent with
  ``Mapping[str, int]``, since there may be additional non-``int``
  values not visible through the type, due to structural subtyping.
  These can be accessed using the ``values()`` and ``items()``
  methods in ``Mapping``, for example.  Example::

      class A(TypedDict):
          x: int

      class B(TypedDict):
          x: int
          y: str

      def sum_values(m: Mapping[str, int]) -> int:
          n = 0
          for v in m.values():
              n += v  # Runtime error
          return n

      def f(a: A) -> None:
          sum_values(a)  # Error: 'A' incompatible with Mapping[str, int]

      b: B = {'x': 0, 'y': 'foo'}
      f(b)


Supported and Unsupported Operations
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Type checkers should support restricted forms of most ``dict``
operations on TypedDict objects.  The guiding principle is that
operations not involving ``Any`` types should be rejected by type
checkers if they may violate runtime type safety.  Here are some of
the most important type safety violations to prevent:

1. A required key is missing.

2. A value has an invalid type.

3. A key that is not defined in the TypedDict type is added.

A key that is not a literal should generally be rejected, since its
value is unknown during type checking, and thus can cause some of the
above violations.  (`Use of Final Values and Literal Types`_
generalizes this to cover final names and literal types.)

The use of a key that is not known to exist should be reported as an
error, even if this wouldn't necessarily generate a runtime type
error.  These are often mistakes, and these may insert values with an
invalid type if structural subtyping hides the types of certain items.
For example, ``d['x'] = 1`` should generate a type check error if
``'x'`` is not a valid key for ``d`` (which is assumed to be a
TypedDict type).

Extra keys included in TypedDict object construction should also be
caught.  In this example, the ``director`` key is not defined in
``Movie`` and is expected to generate an error from a type checker::

    m: Movie = dict(
        name='Alien',
        year=1979,
        director='Ridley Scott')  # error: Unexpected key 'director'

Type checkers should reject the following operations on TypedDict
objects as unsafe, even though they are valid for normal dictionaries:

* Operations with arbitrary ``str`` keys (instead of string literals
  or other expressions with known string values) should generally be
  rejected.  This involves both destructive operations such as setting
  an item and read-only operations such as subscription expressions.
  As an exception to the above rule, ``d.get(e)`` and ``e in d``
  should be allowed for TypedDict objects, for an arbitrary expression
  ``e`` with type ``str``.  The motivation is that these are safe and
  can be useful for introspecting TypedDict objects.  The static type
  of ``d.get(e)`` should be ``object`` if the string value of ``e``
  cannot be determined statically.

* ``clear()`` is not safe since it could remove required keys, some of
  which may not be directly visible because of structural
  subtyping.  ``popitem()`` is similarly unsafe, even if all known
  keys are not required (``total=False``).

* ``del obj['key']`` should be rejected unless ``'key'`` is a
  non-required key.

Type checkers may allow reading an item using ``d['x']`` even if
the key ``'x'`` is not required, instead of requiring the use of
``d.get('x')`` or an explicit ``'x' in d`` check.  The rationale is
that tracking the existence of keys is difficult to implement in full
generality, and that disallowing this could require many changes to
existing code.

The exact type checking rules are up to each type checker to decide.
In some cases potentially unsafe operations may be accepted if the
alternative is to generate false positive errors for idiomatic code.


Use of Final Values and Literal Types
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Type checkers should allow final names (:pep:`591`) with
string values to be used instead of string literals in operations on
TypedDict objects.  For example, this is valid::

   YEAR: Final = 'year'

   m: Movie = {'name': 'Alien', 'year': 1979}
   years_since_epoch = m[YEAR] - 1970

Similarly, an expression with a suitable literal type
(:pep:`586`) can be used instead of a literal value::

   def get_value(movie: Movie,
                 key: Literal['year', 'name']) -> int | str:
       return movie[key]

Type checkers are only expected to support actual string literals, not
final names or literal types, for specifying keys in a TypedDict type
definition.  Also, only a boolean literal can be used to specify
totality in a TypedDict definition.  The motivation for this is to
make type declarations self-contained, and to simplify the
implementation of type checkers.


Backwards Compatibility
^^^^^^^^^^^^^^^^^^^^^^^

To retain backwards compatibility, type checkers should not infer a
TypedDict type unless it is sufficiently clear that this is desired by
the programmer.  When unsure, an ordinary dictionary type should be
inferred.  Otherwise existing code that type checks without errors may
start generating errors once TypedDict support is added to the type
checker, since TypedDict types are more restrictive than dictionary
types.  In particular, they aren't subtypes of dictionary types.


``Required`` and ``NotRequired``
--------------------------------

(Originally specified in :pep:`655`.)

The ``typing.Required`` type qualifier is used to indicate that a
variable declared in a TypedDict definition is a required key:

::

   class Movie(TypedDict, total=False):
       title: Required[str]
       year: int

Additionally the ``typing.NotRequired`` type qualifier is used to
indicate that a variable declared in a TypedDict definition is a
potentially-missing key:

::

   class Movie(TypedDict):  # implicitly total=True
       title: str
       year: NotRequired[int]

It is an error to use ``Required[]`` or ``NotRequired[]`` in any
location that is not an item of a TypedDict.
Type checkers must enforce this restriction.

It is valid to use ``Required[]`` and ``NotRequired[]`` even for
items where it is redundant, to enable additional explicitness if desired:

::

   class Movie(TypedDict):
       title: Required[str]  # redundant
       year: NotRequired[int]

It is an error to use both ``Required[]`` and ``NotRequired[]`` at the
same time:

::

   class Movie(TypedDict):
       title: str
       year: NotRequired[Required[int]]  # ERROR

Type checkers must enforce this restriction.
The runtime implementations of ``Required[]`` and ``NotRequired[]``
may also enforce this restriction.

The :pep:`alternative functional syntax <589#alternative-syntax>`
for TypedDict also supports
``Required[]`` and ``NotRequired[]``:

::

   Movie = TypedDict('Movie', {'name': str, 'year': NotRequired[int]})


Interaction with ``total=False``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Any :pep:`589`-style TypedDict declared with ``total=False`` is equivalent
to a TypedDict with an implicit ``total=True`` definition with all of its
keys marked as ``NotRequired[]``.

Therefore:

::

   class _MovieBase(TypedDict):  # implicitly total=True
       title: str

   class Movie(_MovieBase, total=False):
       year: int


is equivalent to:

::

   class _MovieBase(TypedDict):
       title: str

   class Movie(_MovieBase):
       year: NotRequired[int]


Interaction with ``Annotated[]``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

``Required[]`` and ``NotRequired[]`` can be used with ``Annotated[]``,
in any nesting order:

::

   class Movie(TypedDict):
       title: str
       year: NotRequired[Annotated[int, ValueRange(-9999, 9999)]]  # ok

::

   class Movie(TypedDict):
       title: str
       year: Annotated[NotRequired[int], ValueRange(-9999, 9999)]  # ok

In particular allowing ``Annotated[]`` to be the outermost annotation
for an item allows better interoperability with non-typing uses of
annotations, which may always want ``Annotated[]`` as the outermost annotation
(`discussion <https://bugs.python.org/issue46491>`__).


The ``final`` decorator
-----------------------

(Originally specified in :pep:`591`.)

The ``typing.final`` decorator is used to restrict the use of
inheritance and overriding.

A type checker should prohibit any class decorated with ``@final``
from being subclassed and any method decorated with ``@final`` from
being overridden in a subclass. The method decorator version may be
used with all of instance methods, class methods, static methods, and properties.

For example::

    from typing import final

    @final
    class Base:
        ...

    class Derived(Base):  # Error: Cannot inherit from final class "Base"
        ...

and::

    from typing import final

    class Base:
        @final
        def foo(self) -> None:
            ...

    class Derived(Base):
        def foo(self) -> None:  # Error: Cannot override final attribute "foo"
                                # (previously declared in base class "Base")
            ...


For overloaded methods, ``@final`` should be placed on the
implementation (or on the first overload, for stubs)::

   from typing import Any, overload

   class Base:
       @overload
       def method(self) -> None: ...
       @overload
       def method(self, arg: int) -> int: ...
       @final
       def method(self, x=None):
           ...

It is an error to use ``@final`` on a non-method function.

The ``Final`` annotation
------------------------

(Originally specified in :pep:`591`.)

The ``typing.Final`` type qualifier is used to indicate that a
variable or attribute should not be reassigned, redefined, or overridden.

Syntax
^^^^^^

``Final`` may be used in one of several forms:

* With an explicit type, using the syntax ``Final[<type>]``. Example::

    ID: Final[float] = 1

* With no type annotation. Example::

    ID: Final = 1

  The typechecker should apply its usual type inference mechanisms to
  determine the type of ``ID`` (here, likely, ``int``). Note that unlike for
  generic classes this is *not* the same as ``Final[Any]``.

* In class bodies and stub files you can omit the right hand side and just write
  ``ID: Final[float]``.  If the right hand side is omitted, there must
  be an explicit type argument to ``Final``.

* Finally, as ``self.id: Final = 1`` (also optionally with a type in
  square brackets). This is allowed *only* in ``__init__`` methods, so
  that the final instance attribute is assigned only once when an
  instance is created.


Semantics and examples
^^^^^^^^^^^^^^^^^^^^^^

The two main rules for defining a final name are:

* There can be *at most one* final declaration per module or class for
  a given attribute. There can't be separate class-level and instance-level
  constants with the same name.

* There must be *exactly one* assignment to a final name.

This means a type checker should prevent further assignments to final
names in type-checked code::

   from typing import Final

   RATE: Final = 3000

   class Base:
       DEFAULT_ID: Final = 0

   RATE = 300  # Error: can't assign to final attribute
   Base.DEFAULT_ID = 1  # Error: can't override a final attribute

Note that a type checker need not allow ``Final`` declarations inside loops
since the runtime will see multiple assignments to the same variable in
subsequent iterations.

Additionally, a type checker should prevent final attributes from
being overridden in a subclass::

   from typing import Final

   class Window:
       BORDER_WIDTH: Final = 2.5
       ...

   class ListView(Window):
       BORDER_WIDTH = 3  # Error: can't override a final attribute

A final attribute declared in a class body without an initializer must
be initialized in the ``__init__`` method (except in stub files)::

   class ImmutablePoint:
       x: Final[int]
       y: Final[int]  # Error: final attribute without an initializer

       def __init__(self) -> None:
           self.x = 1  # Good

Type checkers should infer a final attribute that is initialized in
a class body as being a class variable. Variables should not be annotated
with both ``ClassVar`` and ``Final``.

``Final`` may only be used as the outermost type in assignments or variable
annotations. Using it in any other position is an error. In particular,
``Final`` can't be used in annotations for function arguments::

   x: list[Final[int]] = []  # Error!

   def fun(x: Final[List[int]]) ->  None:  # Error!
       ...

Note that declaring a name as final only guarantees that the name will
not be re-bound to another value, but does not make the value
immutable. Immutable ABCs and containers may be used in combination
with ``Final`` to prevent mutating such values::

   x: Final = ['a', 'b']
   x.append('c')  # OK

   y: Final[Sequence[str]] = ['a', 'b']
   y.append('x')  # Error: "Sequence[str]" has no attribute "append"
   z: Final = ('a', 'b')  # Also works


Type checkers should treat uses of a final name that was initialized
with a literal as if it was replaced by the literal. For example, the
following should be allowed::

   from typing import NamedTuple, Final

   X: Final = "x"
   Y: Final = "y"
   N = NamedTuple("N", [(X, int), (Y, int)])

Annotated
---------

(Originally specified by :pep:`593`.)

Syntax
^^^^^^

``Annotated`` is parameterized with a type and an arbitrary list of
Python values that represent the annotations. Here are the specific
details of the syntax:

* The first argument to ``Annotated`` must be a valid type

* Multiple type annotations are supported (``Annotated`` supports variadic
  arguments)::

    Annotated[int, ValueRange(3, 10), ctype("char")]

* ``Annotated`` must be called with at least two arguments (
  ``Annotated[int]`` is not valid)

* The order of the annotations is preserved and matters for equality
  checks::

    Annotated[int, ValueRange(3, 10), ctype("char")] != Annotated[
        int, ctype("char"), ValueRange(3, 10)
    ]

* Nested ``Annotated`` types are flattened, with metadata ordered
  starting with the innermost annotation::

    Annotated[Annotated[int, ValueRange(3, 10)], ctype("char")] == Annotated[
        int, ValueRange(3, 10), ctype("char")
    ]

* Duplicated annotations are not removed::

    Annotated[int, ValueRange(3, 10)] != Annotated[
        int, ValueRange(3, 10), ValueRange(3, 10)
    ]

* ``Annotated`` can be used with nested and generic aliases::

    T = TypeVar("T")
    Vec = Annotated[list[tuple[T, T]], MaxLen(10)]
    V = Vec[int]

    V == Annotated[list[tuple[int, int]], MaxLen(10)]

Consuming annotations
^^^^^^^^^^^^^^^^^^^^^

Ultimately, the responsibility of how to interpret the annotations (if
at all) is the responsibility of the tool or library encountering the
``Annotated`` type. A tool or library encountering an ``Annotated`` type
can scan through the annotations to determine if they are of interest
(e.g., using ``isinstance()``).

**Unknown annotations:** When a tool or a library does not support
annotations or encounters an unknown annotation it should just ignore it
and treat annotated type as the underlying type. For example, when encountering
an annotation that is not an instance of ``struct2.ctype`` to the annotations
for name (e.g., ``Annotated[str, 'foo', struct2.ctype("<10s")]``), the unpack
method should ignore it.

**Namespacing annotations:** Namespaces are not needed for annotations since
the class used by the annotations acts as a namespace.

**Multiple annotations:** It's up to the tool consuming the annotations
to decide whether the client is allowed to have several annotations on
one type and how to merge those annotations.

Since the ``Annotated`` type allows you to put several annotations of
the same (or different) type(s) on any node, the tools or libraries
consuming those annotations are in charge of dealing with potential
duplicates. For example, if you are doing value range analysis you might
allow this::

    T1 = Annotated[int, ValueRange(-10, 5)]
    T2 = Annotated[T1, ValueRange(-20, 3)]

Flattening nested annotations, this translates to::

    T2 = Annotated[int, ValueRange(-10, 5), ValueRange(-20, 3)]

Aliases & Concerns over verbosity
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Writing ``typing.Annotated`` everywhere can be quite verbose;
fortunately, the ability to alias annotations means that in practice we
don't expect clients to have to write lots of boilerplate code::

    T = TypeVar('T')
    Const = Annotated[T, my_annotations.CONST]

    class C:
        def const_method(self: Const[List[int]]) -> int:
            ...


ParamSpec
---------

(Originally specified by :pep:`612`.)

``ParamSpec`` Variables
^^^^^^^^^^^^^^^^^^^^^^^

Declaration
""""""""""""

A parameter specification variable is defined in a similar manner to how a
normal type variable is defined with ``typing.TypeVar``.

.. code-block::

   from typing import ParamSpec
   P = ParamSpec("P")         # Accepted
   P = ParamSpec("WrongName") # Rejected because P =/= WrongName

The runtime should accept ``bound``\ s and ``covariant`` and ``contravariant``
arguments in the declaration just as ``typing.TypeVar`` does, but for now we
will defer the standardization of the semantics of those options to a later PEP.

Valid use locations
"""""""""""""""""""

Previously only a list of parameter arguments (``[A, B, C]``) or an ellipsis
(signifying "undefined parameters") were acceptable as the first "argument" to
``typing.Callable`` .  We now augment that with two new options: a parameter
specification variable (``Callable[P, int]``\ ) or a concatenation on a
parameter specification variable (``Callable[Concatenate[int, P], int]``\ ).

.. code-block::

   callable ::= Callable "[" parameters_expression, type_expression "]"

   parameters_expression ::=
     | "..."
     | "[" [ type_expression ("," type_expression)* ] "]"
     | parameter_specification_variable
     | concatenate "["
                      type_expression ("," type_expression)* ","
                      parameter_specification_variable
                   "]"

where ``parameter_specification_variable`` is a ``typing.ParamSpec`` variable,
declared in the manner as defined above, and ``concatenate`` is
``typing.Concatenate``.

As before, ``parameters_expression``\ s by themselves are not acceptable in
places where a type is expected

.. code-block::

   def foo(x: P) -> P: ...                           # Rejected
   def foo(x: Concatenate[int, P]) -> int: ...       # Rejected
   def foo(x: typing.List[P]) -> None: ...           # Rejected
   def foo(x: Callable[[int, str], P]) -> None: ...  # Rejected


User-Defined Generic Classes
""""""""""""""""""""""""""""

Just as defining a class as inheriting from ``Generic[T]`` makes a class generic
for a single parameter (when ``T`` is a ``TypeVar``\ ), defining a class as
inheriting from ``Generic[P]`` makes a class generic on
``parameters_expression``\ s (when ``P`` is a ``ParamSpec``).

.. code-block::

   T = TypeVar("T")
   P_2 = ParamSpec("P_2")

   class X(Generic[T, P]):
     f: Callable[P, int]
     x: T

   def f(x: X[int, P_2]) -> str: ...                    # Accepted
   def f(x: X[int, Concatenate[int, P_2]]) -> str: ...  # Accepted
   def f(x: X[int, [int, bool]]) -> str: ...            # Accepted
   def f(x: X[int, ...]) -> str: ...                    # Accepted
   def f(x: X[int, int]) -> str: ...                    # Rejected

Or, equivalently, using the built-in syntax for generics in Python 3.12
and higher::

  class X[T, **P]:
    f: Callable[P, int]
    x: T

By the rules defined above, spelling a concrete instance of a class generic
with respect to only a single ``ParamSpec`` would require unsightly double
brackets.  For aesthetic purposes we allow these to be omitted.

.. code-block::

   class Z(Generic[P]):
     f: Callable[P, int]

   def f(x: Z[[int, str, bool]]) -> str: ...   # Accepted
   def f(x: Z[int, str, bool]) -> str: ...     # Equivalent

   # Both Z[[int, str, bool]] and Z[int, str, bool] express this:
   class Z_instantiated:
     f: Callable[[int, str, bool], int]

Semantics
"""""""""

The inference rules for the return type of a function invocation whose signature
contains a ``ParamSpec`` variable are analogous to those around
evaluating ones with ``TypeVar``\ s.

.. code-block::

   def changes_return_type_to_str(x: Callable[P, int]) -> Callable[P, str]: ...

   def returns_int(a: str, b: bool) -> int: ...

   f = changes_return_type_to_str(returns_int) # f should have the type:
                                               # (a: str, b: bool) -> str

   f("A", True)               # Accepted
   f(a="A", b=True)           # Accepted
   f("A", "A")                # Rejected

   expects_str(f("A", True))  # Accepted
   expects_int(f("A", True))  # Rejected

Just as with traditional ``TypeVars``\ , a user may include the same
``ParamSpec`` multiple times in the arguments of the same function,
to indicate a dependency between multiple arguments.  In these cases a type
checker may choose to solve to a common behavioral supertype (i.e. a set of
parameters for which all of the valid calls are valid in both of the subtypes),
but is not obligated to do so.

.. code-block::

   P = ParamSpec("P")

   def foo(x: Callable[P, int], y: Callable[P, int]) -> Callable[P, bool]: ...

   def x_y(x: int, y: str) -> int: ...
   def y_x(y: int, x: str) -> int: ...

   foo(x_y, x_y)  # Should return (x: int, y: str) -> bool

   foo(x_y, y_x)  # Could return (__a: int, __b: str) -> bool
                  # This works because both callables have types that are
                  # behavioral subtypes of Callable[[int, str], int]


   def keyword_only_x(*, x: int) -> int: ...
   def keyword_only_y(*, y: int) -> int: ...
   foo(keyword_only_x, keyword_only_y) # Rejected

The constructors of user-defined classes generic on ``ParamSpec``\ s should be
evaluated in the same way.

.. code-block::

   U = TypeVar("U")

   class Y(Generic[U, P]):
     f: Callable[P, str]
     prop: U

     def __init__(self, f: Callable[P, str], prop: U) -> None:
       self.f = f
       self.prop = prop

   def a(q: int) -> str: ...

   Y(a, 1)   # Should resolve to Y[(q: int), int]
   Y(a, 1).f # Should resolve to (q: int) -> str

The semantics of ``Concatenate[X, Y, P]`` are that it represents the parameters
represented by ``P`` with two positional-only parameters prepended.  This means
that we can use it to represent higher order functions that add, remove or
transform a finite number of parameters of a callable.

.. code-block::

   def bar(x: int, *args: bool) -> int: ...

   def add(x: Callable[P, int]) -> Callable[Concatenate[str, P], bool]: ...

   add(bar)       # Should return (__a: str, x: int, *args: bool) -> bool

   def remove(x: Callable[Concatenate[int, P], int]) -> Callable[P, bool]: ...

   remove(bar)    # Should return (*args: bool) -> bool

   def transform(
     x: Callable[Concatenate[int, P], int]
   ) -> Callable[Concatenate[str, P], bool]: ...

   transform(bar) # Should return (__a: str, *args: bool) -> bool

This also means that while any function that returns an ``R`` can satisfy
``typing.Callable[P, R]``, only functions that can be called positionally in
their first position with a ``X`` can satisfy
``typing.Callable[Concatenate[X, P], R]``.

.. code-block::

   def expects_int_first(x: Callable[Concatenate[int, P], int]) -> None: ...

   @expects_int_first # Rejected
   def one(x: str) -> int: ...

   @expects_int_first # Rejected
   def two(*, x: int) -> int: ...

   @expects_int_first # Rejected
   def three(**kwargs: int) -> int: ...

   @expects_int_first # Accepted
   def four(*args: int) -> int: ...

There are still some classes of decorators still not supported with these
features:

* those that add/remove/change a **variable** number of parameters (for
  example, ``functools.partial`` remains untypable even using ``ParamSpec``)
* those that add/remove/change keyword-only parameters.

The components of a ``ParamSpec``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

A ``ParamSpec`` captures both positional and keyword accessible
parameters, but there unfortunately is no object in the runtime that captures
both of these together. Instead, we are forced to separate them into ``*args``
and ``**kwargs``\ , respectively. This means we need to be able to split apart
a single ``ParamSpec`` into these two components, and then bring
them back together into a call.  To do this, we introduce ``P.args`` to
represent the tuple of positional arguments in a given call and
``P.kwargs`` to represent the corresponding ``Mapping`` of keywords to
values.

Valid use locations
"""""""""""""""""""

These "properties" can only be used as the annotated types for
``*args`` and ``**kwargs``\ , accessed from a ParamSpec already in scope.

.. code-block::

   def puts_p_into_scope(f: Callable[P, int]) -> None:

     def inner(*args: P.args, **kwargs: P.kwargs) -> None:      # Accepted
       pass

     def mixed_up(*args: P.kwargs, **kwargs: P.args) -> None:   # Rejected
       pass

     def misplaced(x: P.args) -> None:                          # Rejected
       pass

   def out_of_scope(*args: P.args, **kwargs: P.kwargs) -> None: # Rejected
     pass


Furthermore, because the default kind of parameter in Python (\ ``(x: int)``\ )
may be addressed both positionally and through its name, two valid invocations
of a ``(*args: P.args, **kwargs: P.kwargs)`` function may give different
partitions of the same set of parameters. Therefore, we need to make sure that
these special types are only brought into the world together, and are used
together, so that our usage is valid for all possible partitions.

.. code-block::

   def puts_p_into_scope(f: Callable[P, int]) -> None:

     stored_args: P.args                           # Rejected

     stored_kwargs: P.kwargs                       # Rejected

     def just_args(*args: P.args) -> None:         # Rejected
       pass

     def just_kwargs(**kwargs: P.kwargs) -> None:  # Rejected
       pass


Semantics
"""""""""

With those requirements met, we can now take advantage of the unique properties
afforded to us by this set up:


* Inside the function, ``args`` has the type ``P.args``\ , not
  ``tuple[P.args, ...]`` as would be with a normal annotation
  (and likewise with the ``**kwargs``\ )

  * This special case is necessary to encapsulate the heterogeneous contents
    of the ``args``/``kwargs`` of a given call, which cannot be expressed
    by an indefinite tuple/dictionary type.

* A function of type ``Callable[P, R]`` can be called with ``(*args, **kwargs)``
  if and only if ``args`` has the type ``P.args`` and ``kwargs`` has the type
  ``P.kwargs``\ , and that those types both originated from the same function
  declaration.
* A function declared as ``def inner(*args: P.args, **kwargs: P.kwargs) -> X``
  has type ``Callable[P, X]``.

With these three properties, we now have the ability to fully type check
parameter preserving decorators.

.. code-block::

   def decorator(f: Callable[P, int]) -> Callable[P, None]:

     def foo(*args: P.args, **kwargs: P.kwargs) -> None:

       f(*args, **kwargs)    # Accepted, should resolve to int

       f(*kwargs, **args)    # Rejected

       f(1, *args, **kwargs) # Rejected

     return foo              # Accepted

To extend this to include ``Concatenate``, we declare the following properties:

* A function of type ``Callable[Concatenate[A, B, P], R]`` can only be
  called with ``(a, b, *args, **kwargs)`` when ``args`` and ``kwargs`` are the
  respective components of ``P``, ``a`` is of type ``A`` and ``b`` is of
  type ``B``.
* A function declared as
  ``def inner(a: A, b: B, *args: P.args, **kwargs: P.kwargs) -> R``
  has type ``Callable[Concatenate[A, B, P], R]``. Placing keyword-only
  parameters between the ``*args`` and ``**kwargs`` is forbidden.

.. code-block::

   def add(f: Callable[P, int]) -> Callable[Concatenate[str, P], None]:

     def foo(s: str, *args: P.args, **kwargs: P.kwargs) -> None:  # Accepted
       pass

     def bar(*args: P.args, s: str, **kwargs: P.kwargs) -> None:  # Rejected
       pass

     return foo                                                   # Accepted


   def remove(f: Callable[Concatenate[int, P], int]) -> Callable[P, None]:

     def foo(*args: P.args, **kwargs: P.kwargs) -> None:
       f(1, *args, **kwargs) # Accepted

       f(*args, 1, **kwargs) # Rejected

       f(*args, **kwargs)    # Rejected

     return foo

Note that the names of the parameters preceding the ``ParamSpec``
components are not mentioned in the resulting ``Concatenate``.  This means that
these parameters can not be addressed via a named argument:

.. code-block::

   def outer(f: Callable[P, None]) -> Callable[P, None]:
     def foo(x: int, *args: P.args, **kwargs: P.kwargs) -> None:
       f(*args, **kwargs)

     def bar(*args: P.args, **kwargs: P.kwargs) -> None:
       foo(1, *args, **kwargs)   # Accepted
       foo(x=1, *args, **kwargs) # Rejected

     return bar

.. _above:

This is not an implementation convenience, but a soundness requirement.  If we
were to allow that second calling style, then the following snippet would be
problematic.

.. code-block::

   @outer
   def problem(*, x: object) -> None:
     pass

   problem(x="uh-oh")

Inside of ``bar``, we would get
``TypeError: foo() got multiple values for argument 'x'``.  Requiring these
concatenated arguments to be addressed positionally avoids this kind of problem,
and simplifies the syntax for spelling these types. Note that this also why we
have to reject signatures of the form
``(*args: P.args, s: str, **kwargs: P.kwargs)``.

If one of these prepended positional parameters contains a free ``ParamSpec``\ ,
we consider that variable in scope for the purposes of extracting the components
of that ``ParamSpec``.  That allows us to spell things like this:

.. code-block::

   def twice(f: Callable[P, int], *args: P.args, **kwargs: P.kwargs) -> int:
     return f(*args, **kwargs) + f(*args, **kwargs)

The type of ``twice`` in the above example is
``Callable[Concatenate[Callable[P, int], P], int]``, where ``P`` is bound by the
outer ``Callable``.  This has the following semantics:

.. code-block::

   def a_int_b_str(a: int, b: str) -> int:
     pass

   twice(a_int_b_str, 1, "A")       # Accepted

   twice(a_int_b_str, b="A", a=1)   # Accepted

   twice(a_int_b_str, "A", 1)       # Rejected


TypeVarTuple
------------

(Originally specified in :pep:`646`.)

In order to support the above use cases, we introduce
``TypeVarTuple``. This serves as a placeholder not for a single type
but for a *tuple* of types.

In addition, we introduce a new use for the star operator: to 'unpack'
``TypeVarTuple`` instances and tuple types such as ``tuple[int,
str]``. Unpacking a ``TypeVarTuple`` or tuple type is the typing
equivalent of unpacking a variable or a tuple of values.

Type Variable Tuples
^^^^^^^^^^^^^^^^^^^^

In the same way that a normal type variable is a stand-in for a single
type such as ``int``, a type variable *tuple* is a stand-in for a *tuple* type such as
``tuple[int, str]``.

Type variable tuples are created and used with:

::

    from typing import TypeVarTuple

    Ts = TypeVarTuple('Ts')

    class Array(Generic[*Ts]):
      ...

    def foo(*args: *Ts):
      ...

Or when using the built-in syntax for generics in Python 3.12 and higher::

    class Array[*Ts]:
      ...
    
    def foo[*Ts](*args: *Ts):
      ...

Using Type Variable Tuples in Generic Classes
"""""""""""""""""""""""""""""""""""""""""""""

Type variable tuples behave like a number of individual type variables packed in a
``tuple``. To understand this, consider the following example:

::

  Shape = TypeVarTuple('Shape')

  class Array(Generic[*Shape]): ...

  Height = NewType('Height', int)
  Width = NewType('Width', int)
  x: Array[Height, Width] = Array()

The ``Shape`` type variable tuple here behaves like ``tuple[T1, T2]``,
where ``T1`` and ``T2`` are type variables. To use these type variables
as type parameters of ``Array``, we must *unpack* the type variable tuple using
the star operator: ``*Shape``. The signature of ``Array`` then behaves
as if we had simply written ``class Array(Generic[T1, T2]): ...``.

In contrast to ``Generic[T1, T2]``, however, ``Generic[*Shape]`` allows
us to parameterise the class with an *arbitrary* number of type parameters.
That is, in addition to being able to define rank-2 arrays such as
``Array[Height, Width]``, we could also define rank-3 arrays, rank-4 arrays,
and so on:

::

  Time = NewType('Time', int)
  Batch = NewType('Batch', int)
  y: Array[Batch, Height, Width] = Array()
  z: Array[Time, Batch, Height, Width] = Array()

Using Type Variable Tuples in Functions
"""""""""""""""""""""""""""""""""""""""

Type variable tuples can be used anywhere a normal ``TypeVar`` can.
This includes class definitions, as shown above, as well as function
signatures and variable annotations:

::

    class Array(Generic[*Shape]):

        def __init__(self, shape: tuple[*Shape]):
            self._shape: tuple[*Shape] = shape

        def get_shape(self) -> tuple[*Shape]:
            return self._shape

    shape = (Height(480), Width(640))
    x: Array[Height, Width] = Array(shape)
    y = abs(x)  # Inferred type is Array[Height, Width]
    z = x + x   #        ...    is Array[Height, Width]

Type Variable Tuples Must Always be Unpacked
""""""""""""""""""""""""""""""""""""""""""""

Note that in the previous example, the ``shape`` argument to ``__init__``
was annotated as ``tuple[*Shape]``. Why is this necessary - if ``Shape``
behaves like ``tuple[T1, T2, ...]``, couldn't we have annotated the ``shape``
argument as ``Shape`` directly?

This is, in fact, deliberately not possible: type variable tuples must
*always* be used unpacked (that is, prefixed by the star operator). This is
for two reasons:

* To avoid potential confusion about whether to use a type variable tuple
  in a packed or unpacked form ("Hmm, should I write '``-> Shape``',
  or '``-> tuple[Shape]``', or '``-> tuple[*Shape]``'...?")
* To improve readability: the star also functions as an explicit visual
  indicator that the type variable tuple is not a normal type variable.

Variance, Type Constraints and Type Bounds: Not (Yet) Supported
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

``TypeVarTuple`` does not yet support specification of:

* Variance (e.g. ``TypeVar('T', covariant=True)``)
* Type constraints (``TypeVar('T', int, float)``)
* Type bounds (``TypeVar('T', bound=ParentClass)``)

We leave the decision of how these arguments should behave to a future PEP, when variadic generics have been tested in the field. As of PEP 646, type variable tuples are
invariant.

Type Variable Tuple Equality
""""""""""""""""""""""""""""

If the same ``TypeVarTuple`` instance is used in multiple places in a signature
or class, a valid type inference might be to bind the ``TypeVarTuple`` to
a ``tuple`` of a union of types:

::

  def foo(arg1: tuple[*Ts], arg2: tuple[*Ts]): ...

  a = (0,)
  b = ('0',)
  foo(a, b)  # Can Ts be bound to tuple[int | str]?

We do *not* allow this; type unions may *not* appear within the ``tuple``.
If a type variable tuple appears in multiple places in a signature,
the types must match exactly (the list of type parameters must be the same
length, and the type parameters themselves must be identical):

::

  def pointwise_multiply(
      x: Array[*Shape],
      y: Array[*Shape]
  ) -> Array[*Shape]: ...

  x: Array[Height]
  y: Array[Width]
  z: Array[Height, Width]
  pointwise_multiply(x, x)  # Valid
  pointwise_multiply(x, y)  # Error
  pointwise_multiply(x, z)  # Error

Multiple Type Variable Tuples: Not Allowed
""""""""""""""""""""""""""""""""""""""""""

Only a single type variable tuple may appear in a type parameter list:

::

    class Array(Generic[*Ts1, *Ts2]): ...  # Error

The reason is that multiple type variable tuples make it ambiguous
which parameters get bound to which type variable tuple: ::

    x: Array[int, str, bool]  # Ts1 = ???, Ts2 = ???

Type Concatenation
^^^^^^^^^^^^^^^^^^

Type variable tuples don't have to be alone; normal types can be
prefixed and/or suffixed:

::

    Shape = TypeVarTuple('Shape')
    Batch = NewType('Batch', int)
    Channels = NewType('Channels', int)

    def add_batch_axis(x: Array[*Shape]) -> Array[Batch, *Shape]: ...
    def del_batch_axis(x: Array[Batch, *Shape]) -> Array[*Shape]: ...
    def add_batch_channels(
      x: Array[*Shape]
    ) -> Array[Batch, *Shape, Channels]: ...

    a: Array[Height, Width]
    b = add_batch_axis(a)      # Inferred type is Array[Batch, Height, Width]
    c = del_batch_axis(b)      # Array[Height, Width]
    d = add_batch_channels(a)  # Array[Batch, Height, Width, Channels]


Normal ``TypeVar`` instances can also be prefixed and/or suffixed:

::

    T = TypeVar('T')
    Ts = TypeVarTuple('Ts')

    def prefix_tuple(
        x: T,
        y: tuple[*Ts]
    ) -> tuple[T, *Ts]: ...

    z = prefix_tuple(x=0, y=(True, 'a'))
    # Inferred type of z is tuple[int, bool, str]

Unpacking Tuple Types
^^^^^^^^^^^^^^^^^^^^^

We mentioned that a ``TypeVarTuple`` stands for a tuple of types.
Since we can unpack a ``TypeVarTuple``, for consistency, we also
allow unpacking a tuple type. As we shall see, this also enables a
number of interesting features.


Unpacking Concrete Tuple Types
""""""""""""""""""""""""""""""

Unpacking a concrete tuple type is analogous to unpacking a tuple of
values at runtime. ``tuple[int, *tuple[bool, bool], str]`` is
equivalent to ``tuple[int, bool, bool, str]``.

Unpacking Unbounded Tuple Types
"""""""""""""""""""""""""""""""

Unpacking an unbounded tuple preserves the unbounded tuple as it is.
That is, ``*tuple[int, ...]`` remains ``*tuple[int, ...]``; there's no
simpler form. This enables us to specify types such as ``tuple[int,
*tuple[str, ...], str]`` - a tuple type where the first element is
guaranteed to be of type ``int``, the last element is guaranteed to be
of type ``str``, and the elements in the middle are zero or more
elements of type ``str``. Note that ``tuple[*tuple[int, ...]]`` is
equivalent to ``tuple[int, ...]``.

Unpacking unbounded tuples is also useful in function signatures where
we don't care about the exact elements and don't want to define an
unnecessary ``TypeVarTuple``:

::

    def process_batch_channels(
        x: Array[Batch, *tuple[Any, ...], Channels]
    ) -> None:
        ...


    x: Array[Batch, Height, Width, Channels]
    process_batch_channels(x)  # OK
    y: Array[Batch, Channels]
    process_batch_channels(y)  # OK
    z: Array[Batch]
    process_batch_channels(z)  # Error: Expected Channels.


We can also pass a ``*tuple[int, ...]`` wherever a ``*Ts`` is
expected. This is useful when we have particularly dynamic code and
cannot state the precise number of dimensions or the precise types for
each of the dimensions. In those cases, we can smoothly fall back to
an unbounded tuple:

::

    y: Array[*tuple[Any, ...]] = read_from_file()

    def expect_variadic_array(
        x: Array[Batch, *Shape]
    ) -> None: ...

    expect_variadic_array(y)  # OK

    def expect_precise_array(
        x: Array[Batch, Height, Width, Channels]
    ) -> None: ...

    expect_precise_array(y)  # OK

``Array[*tuple[Any, ...]]`` stands for an array with an arbitrary
number of dimensions of type ``Any``. This means that, in the call to
``expect_variadic_array``, ``Batch`` is bound to ``Any`` and ``Shape``
is bound to ``tuple[Any, ...]``. In the call to
``expect_precise_array``, the variables ``Batch``, ``Height``,
``Width``, and ``Channels`` are all bound to ``Any``.

This allows users to handle dynamic code gracefully while still
explicitly marking the code as unsafe (by using ``y: Array[*tuple[Any,
...]]``).  Otherwise, users would face noisy errors from the type
checker every time they tried to use the variable ``y``, which would
hinder them when migrating a legacy code base to use ``TypeVarTuple``.

Multiple Unpackings in a Tuple: Not Allowed
"""""""""""""""""""""""""""""""""""""""""""

As with ``TypeVarTuples``, `only one <Multiple Type Variable Tuples:
Not Allowed_>`_ unpacking may appear in a tuple:


::

    x: tuple[int, *Ts, str, *Ts2]  # Error
    y: tuple[int, *tuple[int, ...], str, *tuple[str, ...]]  # Error


``*args`` as a Type Variable Tuple
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

:pep:`484` states that when a type annotation is provided for ``*args``, every argument
must be of the type annotated. That is, if we specify ``*args`` to be type ``int``,
then *all* arguments must be of type ``int``. This limits our ability to specify
the type signatures of functions that take heterogeneous argument types.

If ``*args`` is annotated as a type variable tuple, however, the types of the
individual arguments become the types in the type variable tuple:

::

    Ts = TypeVarTuple('Ts')

    def args_to_tuple(*args: *Ts) -> tuple[*Ts]: ...

    args_to_tuple(1, 'a')  # Inferred type is tuple[int, str]

In the above example, ``Ts`` is bound to ``tuple[int, str]``. If no
arguments are passed, the type variable tuple behaves like an empty
tuple, ``tuple[()]``.

As usual, we can unpack any tuple types. For example, by using a type
variable tuple inside a tuple of other types, we can refer to prefixes
or suffixes of the variadic argument list. For example:

::

    # os.execle takes arguments 'path, arg0, arg1, ..., env'
    def execle(path: str, *args: *tuple[*Ts, Env]) -> None: ...

Note that this is different to

::

    def execle(path: str, *args: *Ts, env: Env) -> None: ...

as this would make ``env`` a keyword-only argument.

Using an unpacked unbounded tuple is equivalent to the
:pep:`484#arbitrary-argument-lists-and-default-argument-values`
behavior of ``*args: int``, which accepts zero or
more values of type ``int``:

::

    def foo(*args: *tuple[int, ...]) -> None: ...

    # equivalent to:
    def foo(*args: int) -> None: ...

Unpacking tuple types also allows more precise types for heterogeneous
``*args``. The following function expects an ``int`` at the beginning,
zero or more ``str`` values, and a ``str`` at the end:

::

    def foo(*args: *tuple[int, *tuple[str, ...], str]) -> None: ...

For completeness, we mention that unpacking a concrete tuple allows us
to specify ``*args`` of a fixed number of heterogeneous types:

::

    def foo(*args: *tuple[int, str]) -> None: ...

    foo(1, "hello")  # OK

Note that, in keeping with the rule that type variable tuples must always
be used unpacked, annotating ``*args`` as being a plain type variable tuple
instance is *not* allowed:

::

    def foo(*args: Ts): ...  # NOT valid

``*args`` is the only case where an argument can be annotated as ``*Ts`` directly;
other arguments should use ``*Ts`` to parameterise something else, e.g. ``tuple[*Ts]``.
If ``*args`` itself is annotated as ``tuple[*Ts]``, the old behaviour still applies:
all arguments must be a ``tuple`` parameterised with the same types.

::

    def foo(*args: tuple[*Ts]): ...

    foo((0,), (1,))    # Valid
    foo((0,), (1, 2))  # Error
    foo((0,), ('1',))  # Error

Finally, note that a type variable tuple may *not* be used as the type of
``**kwargs``. (We do not yet know of a use case for this feature, so we prefer
to leave the ground fresh for a potential future PEP.)

::

    # NOT valid
    def foo(**kwargs: *Ts): ...

Type Variable Tuples with ``Callable``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Type variable tuples can also be used in the arguments section of a
``Callable``:

::

    class Process:
      def __init__(
        self,
        target: Callable[[*Ts], None],
        args: tuple[*Ts],
      ) -> None: ...

    def func(arg1: int, arg2: str) -> None: ...

    Process(target=func, args=(0, 'foo'))  # Valid
    Process(target=func, args=('foo', 0))  # Error

Other types and normal type variables can also be prefixed/suffixed
to the type variable tuple:

::

    T = TypeVar('T')

    def foo(f: Callable[[int, *Ts, T], tuple[T, *Ts]]): ...

The behavior of a Callable containing an unpacked item, whether the
item is a ``TypeVarTuple`` or a tuple type, is to treat the elements
as if they were the type for ``*args``. So, ``Callable[[*Ts], None]``
is treated as the type of the function:

::

    def foo(*args: *Ts) -> None: ...

``Callable[[int, *Ts, T], tuple[T, *Ts]]`` is treated as the type of
the function:

::

    def foo(*args: *tuple[int, *Ts, T]) -> tuple[T, *Ts]: ...

Behaviour when Type Parameters are not Specified
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

When a generic class parameterised by a type variable tuple is used without
any type parameters, it behaves as if the type variable tuple was
substituted with ``tuple[Any, ...]``:

::

    def takes_any_array(arr: Array): ...

    # equivalent to:
    def takes_any_array(arr: Array[*tuple[Any, ...]]): ...

    x: Array[Height, Width]
    takes_any_array(x)  # Valid
    y: Array[Time, Height, Width]
    takes_any_array(y)  # Also valid

This enables gradual typing: existing functions accepting, for example,
a plain TensorFlow ``Tensor`` will still be valid even if ``Tensor`` is made
generic and calling code passes a ``Tensor[Height, Width]``.

This also works in the opposite direction:

::

    def takes_specific_array(arr: Array[Height, Width]): ...

    z: Array
    # equivalent to Array[*tuple[Any, ...]]

    takes_specific_array(z)

(For details, see the section on `Unpacking Unbounded Tuple Types`_.)

This way, even if libraries are updated to use types like ``Array[Height, Width]``,
users of those libraries won't be forced to also apply type annotations to
all of their code; users still have a choice about what parts of their code
to type and which parts to not.

Aliases
^^^^^^^

Generic aliases can be created using a type variable tuple in
a similar way to regular type variables:

::

    IntTuple = tuple[int, *Ts]
    NamedArray = tuple[str, Array[*Ts]]

    IntTuple[float, bool]  # Equivalent to tuple[int, float, bool]
    NamedArray[Height]     # Equivalent to tuple[str, Array[Height]]

As this example shows, all type parameters passed to the alias are
bound to the type variable tuple.

This allows us to define convenience aliases for arrays of a fixed shape
or datatype:

::

    Shape = TypeVarTuple('Shape')
    DType = TypeVar('DType')
    class Array(Generic[DType, *Shape]):

    # E.g. Float32Array[Height, Width, Channels]
    Float32Array = Array[np.float32, *Shape]

    # E.g. Array1D[np.uint8]
    Array1D = Array[DType, Any]

If an explicitly empty type parameter list is given, the type variable
tuple in the alias is set empty:

::

    IntTuple[()]    # Equivalent to tuple[int]
    NamedArray[()]  # Equivalent to tuple[str, Array[()]]

If the type parameter list is omitted entirely, the unspecified type
variable tuples are treated as ``tuple[Any, ...]`` (similar to
`Behaviour when Type Parameters are not Specified`_):

::

    def takes_float_array_of_any_shape(x: Float32Array): ...
    x: Float32Array[Height, Width] = Array()
    takes_float_array_of_any_shape(x)  # Valid

    def takes_float_array_with_specific_shape(
        y: Float32Array[Height, Width]
    ): ...
    y: Float32Array = Array()
    takes_float_array_with_specific_shape(y)  # Valid

Normal ``TypeVar`` instances can also be used in such aliases:

::

    T = TypeVar('T')
    Foo = tuple[T, *Ts]

    # T bound to str, Ts to tuple[int]
    Foo[str, int]
    # T bound to float, Ts to tuple[()]
    Foo[float]
    # T bound to Any, Ts to an tuple[Any, ...]
    Foo


Substitution in Aliases
^^^^^^^^^^^^^^^^^^^^^^^

In the previous section, we only discussed simple usage of generic aliases
in which the type arguments were just simple types. However, a number of
more exotic constructions are also possible.


Type Arguments can be Variadic
""""""""""""""""""""""""""""""

First, type arguments to generic aliases can be variadic. For example, a
``TypeVarTuple`` can be used as a type argument:

::

    Ts1 = TypeVar('Ts1')
    Ts2 = TypeVar('Ts2')

    IntTuple = tuple[int, *Ts1]
    IntFloatTuple = IntTuple[float, *Ts2]  # Valid

Here, ``*Ts1`` in the ``IntTuple`` alias is bound to ``tuple[float, *Ts2]``,
resulting in an alias ``IntFloatTuple`` equivalent to
``tuple[int, float, *Ts2]``.

Unpacked arbitrary-length tuples can also be used as type arguments, with
similar effects:

::

    IntFloatsTuple = IntTuple[*tuple[float, ...]]  # Valid

Here, ``*Ts1`` is bound to ``*tuple[float, ...]``, resulting in
``IntFloatsTuple`` being equivalent to ``tuple[int, *tuple[float, ...]]``: a tuple
consisting of an ``int`` then zero or more ``float``\s.


Variadic Arguments Require Variadic Aliases
"""""""""""""""""""""""""""""""""""""""""""

Variadic type arguments can only be used with generic aliases that are
themselves variadic. For example:

::

    T = TypeVar('T')

    IntTuple = tuple[int, T]

    IntTuple[str]                 # Valid
    IntTuple[*Ts]                 # NOT valid
    IntTuple[*tuple[float, ...]]  # NOT valid

Here, ``IntTuple`` is a *non*-variadic generic alias that takes exactly one
type argument. Hence, it cannot accept ``*Ts`` or ``*tuple[float, ...]`` as type
arguments, because they represent an arbitrary number of types.


Aliases with Both TypeVars and TypeVarTuples
""""""""""""""""""""""""""""""""""""""""""""

In `Aliases`_, we briefly mentioned that aliases can be generic in both
``TypeVar``\s and ``TypeVarTuple``\s:

::

    T = TypeVar('T')
    Foo = tuple[T, *Ts]

    Foo[str, int]         # T bound to str, Ts to tuple[int]
    Foo[str, int, float]  # T bound to str, Ts to tuple[int, float]

In accordance with `Multiple Type Variable Tuples: Not Allowed`_, at most one
``TypeVarTuple`` may appear in the type parameters to an alias. However, a
``TypeVarTuple`` can be combined with an arbitrary number of ``TypeVar``\s,
both before and after:

::

    T1 = TypeVar('T1')
    T2 = TypeVar('T2')
    T3 = TypeVar('T3')

    tuple[*Ts, T1, T2]      # Valid
    tuple[T1, T2, *Ts]      # Valid
    tuple[T1, *Ts, T2, T3]  # Valid

In order to substitute these type variables with supplied type arguments,
any type variables at the beginning or end of the type parameter list first
consume type arguments, and then any remaining type arguments are bound
to the ``TypeVarTuple``:

::

    Shrubbery = tuple[*Ts, T1, T2]

    Shrubbery[str, bool]              # T2=bool,  T1=str,   Ts=tuple[()]
    Shrubbery[str, bool, float]       # T2=float, T1=bool,  Ts=tuple[str]
    Shrubbery[str, bool, float, int]  # T2=int,   T1=float, Ts=tuple[str, bool]

    Ptang = tuple[T1, *Ts, T2, T3]

    Ptang[str, bool, float]       # T1=str, T3=float, T2=bool,  Ts=tuple[()]
    Ptang[str, bool, float, int]  # T1=str, T3=int,   T2=float, Ts=tuple[bool]
       
Note that the minimum number of type arguments in such cases is set by
the number of ``TypeVar``\s:

::

    Shrubbery[int]  # Not valid; Shrubbery needs at least two type arguments


Splitting Arbitrary-Length Tuples
"""""""""""""""""""""""""""""""""

A final complication occurs when an unpacked arbitrary-length tuple is used
as a type argument to an alias consisting of both ``TypeVar``\s and a
``TypeVarTuple``:

::

    Elderberries = tuple[*Ts, T1]
    Hamster = Elderberries[*tuple[int, ...]]  # valid

In such cases, the arbitrary-length tuple is split between the ``TypeVar``\s
and the ``TypeVarTuple``. We assume the arbitrary-length tuple contains
at least as many items as there are ``TypeVar``\s, such that individual
instances of the inner type - here ``int`` - are bound to any ``TypeVar``\s
present. The 'rest' of the arbitrary-length tuple - here ``*tuple[int, ...]``,
since a tuple of arbitrary length minus two items is still arbitrary-length -
is bound to the ``TypeVarTuple``.

Here, therefore, ``Hamster`` is equivalent to ``tuple[*tuple[int, ...], int]``:
a tuple consisting of zero or more ``int``\s, then a final ``int``.

Of course, such splitting only occurs if necessary. For example, if we instead
did:

::

   Elderberries[*tuple[int, ...], str]

Then splitting would not occur; ``T1`` would be bound to ``str``, and
``Ts`` to ``*tuple[int, ...]``.

In particularly awkward cases, a ``TypeVarTuple`` may consume both a type
*and* a part of an arbitrary-length tuple type:

::

    Elderberries[str, *tuple[int, ...]]

Here, ``T1`` is bound to ``int``, and ``Ts`` is bound to
``tuple[str, *tuple[int, ...]]``. This expression is therefore equivalent to
``tuple[str, *tuple[int, ...], int]``: a tuple consisting of a ``str``, then
zero or more ``int``\s, ending with an ``int``.


TypeVarTuples Cannot be Split
"""""""""""""""""""""""""""""

Finally, although any arbitrary-length tuples in the type argument list can be
split between the type variables and the type variable tuple, the same is not
true of ``TypeVarTuple``\s in the argument list:

::

    Ts1 = TypeVarTuple('Ts1')
    Ts2 = TypeVarTuple('Ts2')

    Camelot = tuple[T, *Ts1]
    Camelot[*Ts2]  # NOT valid

This is not possible because, unlike in the case of an unpacked arbitrary-length
tuple, there is no way to 'peer inside' the ``TypeVarTuple`` to see what its
individual types are.


Overloads for Accessing Individual Types
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

For situations where we require access to each individual type in the type variable tuple,
overloads can be used with individual ``TypeVar`` instances in place of the type variable tuple:

::

    Shape = TypeVarTuple('Shape')
    Axis1 = TypeVar('Axis1')
    Axis2 = TypeVar('Axis2')
    Axis3 = TypeVar('Axis3')

    class Array(Generic[*Shape]):

      @overload
      def transpose(
        self: Array[Axis1, Axis2]
      ) -> Array[Axis2, Axis1]: ...

      @overload
      def transpose(
        self: Array[Axis1, Axis2, Axis3]
      ) -> Array[Axis3, Axis2, Axis1]: ...

(For array shape operations in particular, having to specify
overloads for each possible rank is, of course, a rather cumbersome
solution. However, it's the best we can do without additional type
manipulation mechanisms.)


TypeGuard
---------

(Originally specified in :pep:`647`.)

The symbol ``TypeGuard``, exported from the ``typing`` module, is a special form
that accepts a single type argument. It is used to annotate the return type of a
user-defined type guard function. Return statements within a type guard function
should return bool values, and type checkers should verify that all return paths
return a bool.

In all other respects, TypeGuard is a distinct type from bool. It is not a
subtype of bool. Therefore, ``Callable[..., TypeGuard[int]]`` is not assignable
to ``Callable[..., bool]``.

When ``TypeGuard`` is used to annotate the return type of a function or
method that accepts at least one parameter, that function or method is
treated by type checkers as a user-defined type guard. The type argument
provided for ``TypeGuard`` indicates the type that has been validated by
the function.

User-defined type guards can be generic functions, as shown in this example:

::

    _T = TypeVar("_T")

    def is_two_element_tuple(val: Tuple[_T, ...]) -> TypeGuard[tuple[_T, _T]]:
        return len(val) == 2

    def func(names: tuple[str, ...]):
        if is_two_element_tuple(names):
            reveal_type(names)  # tuple[str, str]
        else:
            reveal_type(names)  # tuple[str, ...]


Type checkers should assume that type narrowing should be applied to the
expression that is passed as the first positional argument to a user-defined
type guard. If the type guard function accepts more than one argument, no
type narrowing is applied to those additional argument expressions.

If a type guard function is implemented as an instance method or class method,
the first positional argument maps to the second parameter (after "self" or
"cls").

Here are some examples of user-defined type guard functions that accept more
than one argument:

::

    def is_str_list(val: list[object], allow_empty: bool) -> TypeGuard[list[str]]:
        if len(val) == 0:
            return allow_empty
        return all(isinstance(x, str) for x in val)

    _T = TypeVar("_T")

    def is_set_of(val: set[Any], type: type[_T]) -> TypeGuard[Set[_T]]:
        return all(isinstance(x, type) for x in val)


The return type of a user-defined type guard function will normally refer to
a type that is strictly "narrower" than the type of the first argument (that
is, it's a more specific type that can be assigned to the more general type).
However, it is not required that the return type be strictly narrower. This
allows for cases like the example above where ``list[str]`` is not assignable
to ``list[object]``.

When a conditional statement includes a call to a user-defined type guard
function, and that function returns true, the expression passed as the first 
positional argument to the type guard function should be assumed by a static 
type checker to take on the type specified in the TypeGuard return type, 
unless and until it is further narrowed within the conditional code block.

Some built-in type guards provide narrowing for both positive and negative
tests (in both the ``if`` and ``else`` clauses). For example, consider the
type guard for an expression of the form ``x is None``. If ``x`` has a type that
is a union of None and some other type, it will be narrowed to ``None`` in the
positive case and the other type in the negative case. User-defined type
guards apply narrowing only in the positive case (the ``if`` clause). The type
is not narrowed in the negative case.

::

    OneOrTwoStrs = tuple[str] | tuple[str, str]
    def func(val: OneOrTwoStrs):
        if is_two_element_tuple(val):
            reveal_type(val)  # tuple[str, str]
            ...
        else:
            reveal_type(val)   # OneOrTwoStrs
            ...
        
        if not is_two_element_tuple(val):
            reveal_type(val)   # OneOrTwoStrs
            ...
        else:
            reveal_type(val)  # tuple[str, str]
            ...


``Self``
--------

(Originally specified in :pep:`673`.)

Use in Method Signatures
^^^^^^^^^^^^^^^^^^^^^^^^

``Self`` used in the signature of a method is treated as if it were a
``TypeVar`` bound to the class.

::

    from typing import Self

    class Shape:
        def set_scale(self, scale: float) -> Self:
            self.scale = scale
            return self

is treated equivalently to:

::

    from typing import TypeVar

    SelfShape = TypeVar("SelfShape", bound="Shape")

    class Shape:
        def set_scale(self: SelfShape, scale: float) -> SelfShape:
            self.scale = scale
            return self

This works the same for a subclass too:

::

    class Circle(Shape):
        def set_radius(self, radius: float) -> Self:
            self.radius = radius
            return self

which is treated equivalently to:

::

    SelfCircle = TypeVar("SelfCircle", bound="Circle")

    class Circle(Shape):
        def set_radius(self: SelfCircle, radius: float) -> SelfCircle:
            self.radius = radius
            return self

One implementation strategy is to simply desugar the former to the latter in a
preprocessing step. If a method uses ``Self`` in its signature, the type of
``self`` within a method will be ``Self``. In other cases, the type of
``self`` will remain the enclosing class.


Use in Classmethod Signatures
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The ``Self`` type annotation is also useful for classmethods that return
an instance of the class that they operate on. For example, ``from_config`` in
the following snippet builds a ``Shape`` object from a given ``config``.

::

    class Shape:
        def __init__(self, scale: float) -> None: ...

        @classmethod
        def from_config(cls, config: dict[str, float]) -> Shape:
            return cls(config["scale"])


However, this means that ``Circle.from_config(...)`` is inferred to return a
value of type ``Shape``, when in fact it should be ``Circle``:

::

    class Circle(Shape):
        def circumference(self) -> float: ...

    shape = Shape.from_config({"scale": 7.0})
    # => Shape

    circle = Circle.from_config({"scale": 7.0})
    # => *Shape*, not Circle

    circle.circumference()
    # Error: `Shape` has no attribute `circumference`


The current workaround for this is unintuitive and error-prone:

::

    Self = TypeVar("Self", bound="Shape")

    class Shape:
        @classmethod
        def from_config(
            cls: type[Self], config: dict[str, float]
        ) -> Self:
            return cls(config["scale"])

Instead, ``Self`` can be used directly:

::

    from typing import Self

    class Shape:
        @classmethod
        def from_config(cls, config: dict[str, float]) -> Self:
            return cls(config["scale"])

This avoids the complicated ``cls: type[Self]`` annotation and the ``TypeVar``
declaration with a ``bound``. Once again, the latter code behaves equivalently
to the former code.

Use in Parameter Types
^^^^^^^^^^^^^^^^^^^^^^

Another use for ``Self`` is to annotate parameters that expect instances of
the current class:

::

    Self = TypeVar("Self", bound="Shape")

    class Shape:
        def difference(self: Self, other: Self) -> float: ...

        def apply(self: Self, f: Callable[[Self], None]) -> None: ...

``Self`` can be used directly to achieve the same behavior:

::

    from typing import Self

    class Shape:
        def difference(self, other: Self) -> float: ...

        def apply(self, f: Callable[[Self], None]) -> None: ...

Note that specifying ``self: Self`` is harmless, so some users may find it
more readable to write the above as:

::

    class Shape:
        def difference(self: Self, other: Self) -> float: ...

Use in Attribute Annotations
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Another use for ``Self`` is to annotate attributes. One example is where we
have a ``LinkedList`` whose elements must be subclasses of the current class.

::

    from dataclasses import dataclass
    from typing import Generic, TypeVar

    T = TypeVar("T")

    @dataclass
    class LinkedList(Generic[T]):
        value: T
        next: LinkedList[T] | None = None

    # OK
    LinkedList[int](value=1, next=LinkedList[int](value=2))
    # Not OK
    LinkedList[int](value=1, next=LinkedList[str](value="hello"))


However, annotating the ``next`` attribute as ``LinkedList[T]`` allows invalid
constructions with subclasses:

::

    @dataclass
    class OrdinalLinkedList(LinkedList[int]):
        def ordinal_value(self) -> str:
            return as_ordinal(self.value)

    # Should not be OK because LinkedList[int] is not a subclass of
    # OrdinalLinkedList, # but the type checker allows it.
    xs = OrdinalLinkedList(value=1, next=LinkedList[int](value=2))

    if xs.next:
        print(xs.next.ordinal_value())  # Runtime Error.


This constraint can be expressed using ``next: Self | None``:

::

    from typing import Self

    @dataclass
    class LinkedList(Generic[T]):
        value: T
        next: Self | None = None

    @dataclass
    class OrdinalLinkedList(LinkedList[int]):
        def ordinal_value(self) -> str:
            return as_ordinal(self.value)

    xs = OrdinalLinkedList(value=1, next=LinkedList[int](value=2))
    # Type error: Expected OrdinalLinkedList, got LinkedList[int].

    if xs.next is not None:
        xs.next = OrdinalLinkedList(value=3, next=None)  # OK
        xs.next = LinkedList[int](value=3, next=None)  # Not OK



The code above is semantically equivalent to treating each attribute
containing a ``Self`` type as a ``property`` that returns that type:

::

    from dataclasses import dataclass
    from typing import Any, Generic, TypeVar

    T = TypeVar("T")
    Self = TypeVar("Self", bound="LinkedList")


    class LinkedList(Generic[T]):
        value: T

        @property
        def next(self: Self) -> Self | None:
            return self._next

        @next.setter
        def next(self: Self, next: Self | None) -> None:
            self._next = next

    class OrdinalLinkedList(LinkedList[int]):
        def ordinal_value(self) -> str:
            return str(self.value)

Use in Generic Classes
^^^^^^^^^^^^^^^^^^^^^^

``Self`` can also be used in generic class methods:

::

    class Container(Generic[T]):
        value: T
        def set_value(self, value: T) -> Self: ...


This is equivalent to writing:

::

    Self = TypeVar("Self", bound="Container[Any]")

    class Container(Generic[T]):
        value: T
        def set_value(self: Self, value: T) -> Self: ...


The behavior is to preserve the type argument of the object on which the
method was called. When called on an object with concrete type
``Container[int]``, ``Self`` is bound to ``Container[int]``. When called with
an object of generic type ``Container[T]``, ``Self`` is bound to
``Container[T]``:

::

    def object_with_concrete_type() -> None:
        int_container: Container[int]
        str_container: Container[str]
        reveal_type(int_container.set_value(42))  # => Container[int]
        reveal_type(str_container.set_value("hello"))  # => Container[str]

    def object_with_generic_type(
        container: Container[T], value: T,
    ) -> Container[T]:
        return container.set_value(value)  # => Container[T]


The PEP doesn’t specify the exact type of ``self.value`` within the method
``set_value``. Some type checkers may choose to implement ``Self`` types using
class-local type variables with ``Self = TypeVar(“Self”,
bound=Container[T])``, which will infer a precise type ``T``. However, given
that class-local type variables are not a standardized type system feature, it
is also acceptable to infer ``Any`` for ``self.value``. We leave this up to
the type checker.

Note that we reject using ``Self`` with type arguments, such as ``Self[int]``.
This is because it creates ambiguity about the type of the ``self`` parameter
and introduces unnecessary complexity:

::

    class Container(Generic[T]):
        def foo(
            self, other: Self[int], other2: Self,
        ) -> Self[str]:  # Rejected
            ...

In such cases, we recommend using an explicit type for ``self``:

::

    class Container(Generic[T]):
        def foo(
            self: Container[T],
            other: Container[int],
            other2: Container[T]
        ) -> Container[str]: ...


Use in Protocols
^^^^^^^^^^^^^^^^

``Self`` is valid within Protocols, similar to its use in classes:

::

    from typing import Protocol, Self

    class ShapeProtocol(Protocol):
        scale: float

        def set_scale(self, scale: float) -> Self:
            self.scale = scale
            return self

is treated equivalently to:

::

    from typing import TypeVar

    SelfShape = TypeVar("SelfShape", bound="ShapeProtocol")

    class ShapeProtocol(Protocol):
        scale: float

        def set_scale(self: SelfShape, scale: float) -> SelfShape:
            self.scale = scale
            return self


See :pep:`PEP 544
<544#self-types-in-protocols>` for
details on the behavior of TypeVars bound to protocols.

Checking a class for compatibility with a protocol: If a protocol uses
``Self`` in methods or attribute annotations, then a class ``Foo`` is
considered compatible with the protocol if its corresponding methods and
attribute annotations use either ``Self`` or ``Foo`` or any of ``Foo``’s
subclasses. See the examples below:

::

    from typing import Protocol

    class ShapeProtocol(Protocol):
        def set_scale(self, scale: float) -> Self: ...

    class ReturnSelf:
        scale: float = 1.0

        def set_scale(self, scale: float) -> Self:
            self.scale = scale
            return self

    class ReturnConcreteShape:
        scale: float = 1.0

        def set_scale(self, scale: float) -> ReturnConcreteShape:
            self.scale = scale
            return self

    class BadReturnType:
        scale: float = 1.0

        def set_scale(self, scale: float) -> int:
            self.scale = scale
            return 42

    class ReturnDifferentClass:
        scale: float = 1.0

        def set_scale(self, scale: float) -> ReturnConcreteShape:
            return ReturnConcreteShape(...)

    def accepts_shape(shape: ShapeProtocol) -> None:
        y = shape.set_scale(0.5)
        reveal_type(y)

    def main() -> None:
        return_self_shape: ReturnSelf
        return_concrete_shape: ReturnConcreteShape
        bad_return_type: BadReturnType
        return_different_class: ReturnDifferentClass

        accepts_shape(return_self_shape)  # OK
        accepts_shape(return_concrete_shape)  # OK
        accepts_shape(bad_return_type)  # Not OK
        # Not OK because it returns a non-subclass.
        accepts_shape(return_different_class)


Valid Locations for ``Self``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

A ``Self`` annotation is only valid in class contexts, and will always refer
to the encapsulating class. In contexts involving nested classes, ``Self``
will always refer to the innermost class.

The following uses of ``Self`` are accepted:

::

    class ReturnsSelf:
        def foo(self) -> Self: ... # Accepted

        @classmethod
        def bar(cls) -> Self:  # Accepted
            return cls()

        def __new__(cls, value: int) -> Self: ...  # Accepted

        def explicitly_use_self(self: Self) -> Self: ...  # Accepted

        # Accepted (Self can be nested within other types)
        def returns_list(self) -> list[Self]: ...

        # Accepted (Self can be nested within other types)
        @classmethod
        def return_cls(cls) -> type[Self]:
            return cls

    class Child(ReturnsSelf):
        # Accepted (we can override a method that uses Self annotations)
        def foo(self) -> Self: ...

    class TakesSelf:
        def foo(self, other: Self) -> bool: ...  # Accepted

    class Recursive:
        # Accepted (treated as an @property returning ``Self | None``)
        next: Self | None

    class CallableAttribute:
        def foo(self) -> int: ...

        # Accepted (treated as an @property returning the Callable type)
        bar: Callable[[Self], int] = foo

    class HasNestedFunction:
        x: int = 42

        def foo(self) -> None:

            # Accepted (Self is bound to HasNestedFunction).
            def nested(z: int, inner_self: Self) -> Self:
                print(z)
                print(inner_self.x)
                return inner_self

            nested(42, self)  # OK


    class Outer:
        class Inner:
            def foo(self) -> Self: ...  # Accepted (Self is bound to Inner)


The following uses of ``Self`` are rejected.

::

    def foo(bar: Self) -> Self: ...  # Rejected (not within a class)

    bar: Self  # Rejected (not within a class)

    class Foo:
        # Rejected (Self is treated as unknown).
        def has_existing_self_annotation(self: T) -> Self: ...

    class Foo:
        def return_concrete_type(self) -> Self:
            return Foo()  # Rejected (see FooChild below for rationale)

    class FooChild(Foo):
        child_value: int = 42

        def child_method(self) -> None:
            # At runtime, this would be Foo, not FooChild.
            y = self.return_concrete_type()

            y.child_value
            # Runtime error: Foo has no attribute child_value

    class Bar(Generic[T]):
        def bar(self) -> T: ...

    class Baz(Bar[Self]): ...  # Rejected

We reject type aliases containing ``Self``. Supporting ``Self``
outside class definitions can require a lot of special-handling in
type checkers. Given that it also goes against the rest of the PEP to
use ``Self`` outside a class definition, we believe the added
convenience of aliases is not worth it:

::

    TupleSelf = Tuple[Self, Self]  # Rejected

    class Alias:
        def return_tuple(self) -> TupleSelf:  # Rejected
            return (self, self)

Note that we reject ``Self`` in staticmethods. ``Self`` does not add much
value since there is no ``self`` or ``cls`` to return. The only possible use
cases would be to return a parameter itself or some element from a container
passed in as a parameter. These don’t seem worth the additional complexity.

::

    class Base:
        @staticmethod
        def make() -> Self:  # Rejected
            ...

        @staticmethod
        def return_parameter(foo: Self) -> Self:  # Rejected
            ...

Likewise, we reject ``Self`` in metaclasses. ``Self`` consistently refers to the
same type (that of ``self``). But in metaclasses, it would have to refer to
different types in different method signatures. For example, in ``__mul__``,
``Self`` in the return type would refer to the implementing class
``Foo``, not the enclosing class ``MyMetaclass``. But, in ``__new__``, ``Self``
in the return type would refer to the enclosing class ``MyMetaclass``. To
avoid confusion, we reject this edge case.

::

    class MyMetaclass(type):
        def __new__(cls, *args: Any) -> Self:  # Rejected
            return super().__new__(cls, *args)

        def __mul__(cls, count: int) -> list[Self]:  # Rejected
            return [cls()] * count

    class Foo(metaclass=MyMetaclass): ...

The ``dataclass_transform`` decorator
-------------------------------------

(Originally specified in :pep:`681`.)

Specification
^^^^^^^^^^^^^

This specification describes a decorator function in
the ``typing`` module named ``dataclass_transform``. This decorator
can be applied to either a function that is itself a decorator,
a class, or a metaclass. The presence of
``dataclass_transform`` tells a static type checker that the decorated
function, class, or metaclass performs runtime "magic" that transforms
a class, endowing it with dataclass-like behaviors.

If ``dataclass_transform`` is applied to a function, using the decorated
function as a decorator is assumed to apply dataclass-like semantics.
If the function has overloads, the ``dataclass_transform`` decorator can
be applied to the implementation of the function or any one, but not more
than one, of the overloads. When applied to an overload, the
``dataclass_transform`` decorator still impacts all usage of the
function.

If ``dataclass_transform`` is applied to a class, dataclass-like
semantics will be assumed for any class that directly or indirectly
derives from the decorated class or uses the decorated class as a
metaclass. Attributes on the decorated class and its base classes
are not considered to be fields.

Examples of each approach are shown in the following sections. Each
example creates a ``CustomerModel`` class with dataclass-like semantics.
The implementation of the decorated objects is omitted for brevity,
but we assume that they modify classes in the following ways:

* They synthesize an ``__init__`` method using data fields declared
  within the class and its parent classes.
* They synthesize ``__eq__`` and ``__ne__`` methods.

Type checkers will recognize that the
``CustomerModel`` class can be instantiated using the synthesized
``__init__`` method:

.. code-block:: python

  # Using positional arguments
  c1 = CustomerModel(327, "John Smith")

  # Using keyword arguments
  c2 = CustomerModel(id=327, name="John Smith")

  # These calls will generate runtime errors and should be flagged as
  # errors by a static type checker.
  c3 = CustomerModel()
  c4 = CustomerModel(327, first_name="John")
  c5 = CustomerModel(327, "John Smith", 0)

Decorator function example
""""""""""""""""""""""""""

.. code-block:: python

  _T = TypeVar("_T")
  
  # The ``create_model`` decorator is defined by a library.
  # This could be in a type stub or inline.
  @typing.dataclass_transform()
  def create_model(cls: Type[_T]) -> Type[_T]:
      cls.__init__ = ...
      cls.__eq__ = ...
      cls.__ne__ = ...
      return cls
  
  # The ``create_model`` decorator can now be used to create new model
  # classes, like this:
  @create_model
  class CustomerModel:
      id: int
      name: str

Class example
"""""""""""""

.. code-block:: python

  # The ``ModelBase`` class is defined by a library. This could be in
  # a type stub or inline.
  @typing.dataclass_transform()
  class ModelBase: ...

  # The ``ModelBase`` class can now be used to create new model
  # subclasses, like this:
  class CustomerModel(ModelBase):
      id: int
      name: str

Metaclass example
"""""""""""""""""

.. code-block:: python

  # The ``ModelMeta`` metaclass and ``ModelBase`` class are defined by
  # a library. This could be in a type stub or inline.
  @typing.dataclass_transform()
  class ModelMeta(type): ...
  
  class ModelBase(metaclass=ModelMeta): ...
  
  # The ``ModelBase`` class can now be used to create new model
  # subclasses, like this:
  class CustomerModel(ModelBase):
      id: int
      name: str

Decorator function and class/metaclass parameters
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

A decorator function, class, or metaclass that provides dataclass-like
functionality may accept parameters that modify certain behaviors.
This specification defines the following parameters that static type
checkers must honor if they are used by a dataclass transform. Each of
these parameters accepts a bool argument, and it must be possible for
the bool value (``True`` or ``False``) to be statically evaluated.

* ``eq``,  ``order``, ``frozen``, ``init`` and ``unsafe_hash`` are parameters
  supported in the stdlib dataclass, with meanings defined in 
  :pep:`PEP 557 <557#id7>`.
* ``kw_only``, ``match_args`` and ``slots`` are parameters supported
  in the stdlib dataclass, first introduced in Python 3.10.

``dataclass_transform`` parameters
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Parameters to ``dataclass_transform`` allow for some basic
customization of default behaviors:

.. code-block:: python

  _T = TypeVar("_T")
  
  def dataclass_transform(
      *,
      eq_default: bool = True,
      order_default: bool = False,
      kw_only_default: bool = False,
      field_specifiers: tuple[type | Callable[..., Any], ...] = (),
      **kwargs: Any,
  ) -> Callable[[_T], _T]: ...

* ``eq_default`` indicates whether the ``eq`` parameter is assumed to
  be True or False if it is omitted by the caller. If not specified,
  ``eq_default`` will default to True (the default assumption for
  dataclass).
* ``order_default`` indicates whether the ``order`` parameter is
  assumed to be True or False if it is omitted by the caller. If not
  specified, ``order_default`` will default to False (the default
  assumption for dataclass).
* ``kw_only_default`` indicates whether the ``kw_only`` parameter is
  assumed to be True or False if it is omitted by the caller. If not
  specified, ``kw_only_default`` will default to False (the default
  assumption for dataclass).
* ``field_specifiers`` specifies a static list of supported classes
  that describe fields. Some libraries also supply functions to
  allocate instances of field specifiers, and those functions may
  also be specified in this tuple. If not specified,
  ``field_specifiers`` will default to an empty tuple (no field
  specifiers supported). The standard dataclass behavior supports
  only one type of field specifier called ``Field`` plus a helper
  function (``field``) that instantiates this class, so if we were
  describing the stdlib dataclass behavior, we would provide the
  tuple argument ``(dataclasses.Field, dataclasses.field)``.
* ``kwargs`` allows arbitrary additional keyword args to be passed to
  ``dataclass_transform``. This gives type checkers the freedom to
  support experimental parameters without needing to wait for changes
  in ``typing.py``. Type checkers should report errors for any
  unrecognized parameters.

In the future, we may add additional parameters to
``dataclass_transform`` as needed to support common behaviors in user
code. These additions will be made after reaching consensus on
typing-sig rather than via additional PEPs.

The following sections provide additional examples showing how these
parameters are used.

Decorator function example
""""""""""""""""""""""""""

.. code-block:: python

  # Indicate that the ``create_model`` function assumes keyword-only
  # parameters for the synthesized ``__init__`` method unless it is
  # invoked with ``kw_only=False``. It always synthesizes order-related
  # methods and provides no way to override this behavior.
  @typing.dataclass_transform(kw_only_default=True, order_default=True)
  def create_model(
      *,
      frozen: bool = False,
      kw_only: bool = True,
  ) -> Callable[[Type[_T]], Type[_T]]: ...
  
  # Example of how this decorator would be used by code that imports
  # from this library:
  @create_model(frozen=True, kw_only=False)
  class CustomerModel:
      id: int
      name: str

Class example
"""""""""""""

.. code-block:: python

  # Indicate that classes that derive from this class default to
  # synthesizing comparison methods.
  @typing.dataclass_transform(eq_default=True, order_default=True)
  class ModelBase:
      def __init_subclass__(
          cls,
          *,
          init: bool = True,
          frozen: bool = False,
          eq: bool = True,
          order: bool = True,
      ):
          ...
  
  # Example of how this class would be used by code that imports
  # from this library:
  class CustomerModel(
      ModelBase,
      init=False,
      frozen=True,
      eq=False,
      order=False,
  ):
      id: int
      name: str

Metaclass example
"""""""""""""""""

.. code-block:: python

  # Indicate that classes that use this metaclass default to
  # synthesizing comparison methods.
  @typing.dataclass_transform(eq_default=True, order_default=True)
  class ModelMeta(type):
      def __new__(
          cls,
          name,
          bases,
          namespace,
          *,
          init: bool = True,
          frozen: bool = False,
          eq: bool = True,
          order: bool = True,
      ):
          ...
  
  class ModelBase(metaclass=ModelMeta):
      ...
  
  # Example of how this class would be used by code that imports
  # from this library:
  class CustomerModel(
      ModelBase,
      init=False,
      frozen=True,
      eq=False,
      order=False,
  ):
      id: int
      name: str


Field specifiers
^^^^^^^^^^^^^^^^^

Most libraries that support dataclass-like semantics provide one or
more "field specifier" types that allow a class definition to provide
additional metadata about each field in the class. This metadata can
describe, for example, default values, or indicate whether the field
should be included in the synthesized ``__init__`` method.

Field specifiers can be omitted in cases where additional metadata is
not required:

.. code-block:: python

  @dataclass
  class Employee:
      # Field with no specifier
      name: str
  
      # Field that uses field specifier class instance
      age: Optional[int] = field(default=None, init=False)
  
      # Field with type annotation and simple initializer to
      # describe default value
      is_paid_hourly: bool = True
  
      # Not a field (but rather a class variable) because type
      # annotation is not provided.
      office_number = "unassigned"


Field specifier parameters
""""""""""""""""""""""""""

Libraries that support dataclass-like semantics and support field
specifier classes typically use common parameter names to construct
these field specifiers. This specification formalizes the names and
meanings of the parameters that must be understood for static type
checkers. These standardized parameters must be keyword-only.

These parameters are a superset of those supported by
``dataclasses.field``, excluding those that do not have an impact on
type checking such as ``compare`` and ``hash``.

Field specifier classes are allowed to use other
parameters in their constructors, and those parameters can be
positional and may use other names.

* ``init`` is an optional bool parameter that indicates whether the
  field should be included in the synthesized ``__init__`` method. If
  unspecified, ``init`` defaults to True. Field specifier functions
  can use overloads that implicitly specify the value of ``init``
  using a literal bool value type
  (``Literal[False]`` or ``Literal[True]``).
* ``default`` is an optional parameter that provides the default value
  for the field.
* ``default_factory`` is an optional parameter that provides a runtime
  callback that returns the default value for the field. If neither
  ``default`` nor ``default_factory`` are specified, the field is
  assumed to have no default value and must be provided a value when
  the class is instantiated.
* ``factory`` is an alias for ``default_factory``. Stdlib dataclasses
  use the name ``default_factory``, but attrs uses the name ``factory``
  in many scenarios, so this alias is necessary for supporting attrs.
* ``kw_only`` is an optional bool parameter that indicates whether the
  field should be marked as keyword-only. If true, the field will be
  keyword-only. If false, it will not be keyword-only. If unspecified,
  the value of the ``kw_only`` parameter on the object decorated with
  ``dataclass_transform`` will be used, or if that is unspecified, the
  value of ``kw_only_default`` on ``dataclass_transform`` will be used.
* ``alias`` is an optional str parameter that provides an alternative
  name for the field. This alternative name is used in the synthesized
  ``__init__`` method.

It is an error to specify more than one of ``default``,
``default_factory`` and ``factory``.

This example demonstrates the above:

.. code-block:: python

  # Library code (within type stub or inline)
  # In this library, passing a resolver means that init must be False,
  # and the overload with Literal[False] enforces that.
  @overload
  def model_field(
          *,
          default: Optional[Any] = ...,
          resolver: Callable[[], Any],
          init: Literal[False] = False,
      ) -> Any: ...
  
  @overload
  def model_field(
          *,
          default: Optional[Any] = ...,
          resolver: None = None,
          init: bool = True,
      ) -> Any: ...
  
  @typing.dataclass_transform(
      kw_only_default=True,
      field_specifiers=(model_field, ))
  def create_model(
      *,
      init: bool = True,
  ) -> Callable[[Type[_T]], Type[_T]]: ...
  
  # Code that imports this library:
  @create_model(init=False)
  class CustomerModel:
      id: int = model_field(resolver=lambda : 0)
      name: str


Runtime behavior
^^^^^^^^^^^^^^^^

At runtime, the ``dataclass_transform`` decorator's only effect is to
set an attribute named ``__dataclass_transform__`` on the decorated
function or class to support introspection. The value of the attribute
should be a dict mapping the names of the ``dataclass_transform``
parameters to their values.

For example:

.. code-block:: python

  {
    "eq_default": True,
    "order_default": False,
    "kw_only_default": False,
    "field_specifiers": (),
    "kwargs": {}
  }


Dataclass semantics
^^^^^^^^^^^^^^^^^^^

Except where stated otherwise, classes impacted by
``dataclass_transform``, either by inheriting from a class that is
decorated with ``dataclass_transform`` or by being decorated with
a function decorated with ``dataclass_transform``, are assumed to
behave like stdlib ``dataclass``.

This includes, but is not limited to, the following semantics:

* Frozen dataclasses cannot inherit from non-frozen dataclasses. A
  class that has been decorated with ``dataclass_transform`` is
  considered neither frozen nor non-frozen, thus allowing frozen
  classes to inherit from it. Similarly, a class that directly
  specifies a metaclass that is decorated with ``dataclass_transform``
  is considered neither frozen nor non-frozen.

  Consider these class examples:
   
  .. code-block:: python

    # ModelBase is not considered either "frozen" or "non-frozen"
    # because it is decorated with ``dataclass_transform``
    @typing.dataclass_transform()
    class ModelBase(): ...

    # Vehicle is considered non-frozen because it does not specify
    # "frozen=True".
    class Vehicle(ModelBase):
        name: str

    # Car is a frozen class that derives from Vehicle, which is a
    # non-frozen class. This is an error.
    class Car(Vehicle, frozen=True):
        wheel_count: int

  And these similar metaclass examples:
   
  .. code-block:: python

    @typing.dataclass_transform()
    class ModelMeta(type): ...

    # ModelBase is not considered either "frozen" or "non-frozen"
    # because it directly specifies ModelMeta as its metaclass.
    class ModelBase(metaclass=ModelMeta): ...

    # Vehicle is considered non-frozen because it does not specify
    # "frozen=True".
    class Vehicle(ModelBase):
        name: str

    # Car is a frozen class that derives from Vehicle, which is a
    # non-frozen class. This is an error.
    class Car(Vehicle, frozen=True):
        wheel_count: int

* Field ordering and inheritance is assumed to follow the rules
  specified in :pep:`557 <557#inheritance>`. This includes the effects of
  overrides (redefining a field in a child class that has already been
  defined in a parent class).

* :pep:`PEP 557 indicates <557#post-init-parameters>` that
  all fields without default values must appear before
  fields with default values. Although not explicitly
  stated in PEP 557, this rule is ignored when ``init=False``, and
  this specification likewise ignores this requirement in that
  situation. Likewise, there is no need to enforce this ordering when
  keyword-only parameters are used for ``__init__``, so the rule is
  not enforced if ``kw_only`` semantics are in effect.

* As with ``dataclass``, method synthesis is skipped if it would
  overwrite a method that is explicitly declared within the class.
  Method declarations on base classes do not cause method synthesis to
  be skipped.

  For example, if a class declares an ``__init__`` method explicitly,
  an ``__init__`` method will not be synthesized for that class.

* KW_ONLY sentinel values are supported as described in `the Python
  docs <https://docs.python.org/3/library/dataclasses.html#dataclasses.KW_ONLY>`_
  and `bpo-43532 <https://bugs.python.org/issue43532>`_.

* ClassVar attributes are not considered dataclass fields and are
  `ignored by dataclass mechanisms <https://docs.python.org/3/library/dataclasses.html#class-variables>`_.


Undefined behavior
^^^^^^^^^^^^^^^^^^

If multiple ``dataclass_transform`` decorators are found, either on a
single function (including its overloads), a single class, or within a
class hierarchy, the resulting behavior is undefined. Library authors
should avoid these scenarios.


``Unpack``
----------

``typing.Unpack`` has two use cases in the type system:

* As introduced by :pep:`646`, a backward-compatible form for certain operations
  involving variadic generics. See the section on ``TypeVarTuple`` for details.
* As introduced by :pep:`692`, a way to annotate the ``**kwargs`` of a function.

This second usage is described in this section. The following example::

    from typing import TypedDict, Unpack

    class Movie(TypedDict):
        name: str
        year: int

    def foo(**kwargs: Unpack[Movie]) -> None: ...

means that the ``**kwargs`` comprise two keyword arguments specified by
``Movie`` (i.e. a ``name`` keyword of type ``str`` and a ``year`` keyword of
type ``int``). This indicates that the function should be called as follows::

    kwargs: Movie = {"name": "Life of Brian", "year": 1979}

    foo(**kwargs)                               # OK!
    foo(name="The Meaning of Life", year=1983)  # OK!

When ``Unpack`` is used, type checkers treat ``kwargs`` inside the
function body as a ``TypedDict``::

    def foo(**kwargs: Unpack[Movie]) -> None:
        assert_type(kwargs, Movie)  # OK!


Using the new annotation will not have any runtime effect - it should only be
taken into account by type checkers. Any mention of errors in the following
sections relates to type checker errors.

Function calls with standard dictionaries
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Passing a dictionary of type ``dict[str, object]`` as a ``**kwargs`` argument
to a function that has ``**kwargs`` annotated with ``Unpack`` must generate a
type checker error. On the other hand, the behaviour for functions using
standard, untyped dictionaries can depend on the type checker. For example::

    def foo(**kwargs: Unpack[Movie]) -> None: ...

    movie: dict[str, object] = {"name": "Life of Brian", "year": 1979}
    foo(**movie)  # WRONG! Movie is of type dict[str, object]

    typed_movie: Movie = {"name": "The Meaning of Life", "year": 1983}
    foo(**typed_movie)  # OK!

    another_movie = {"name": "Life of Brian", "year": 1979}
    foo(**another_movie)  # Depends on the type checker.

Keyword collisions
^^^^^^^^^^^^^^^^^^

A ``TypedDict`` that is used to type ``**kwargs`` could potentially contain
keys that are already defined in the function's signature. If the duplicate
name is a standard parameter, an error should be reported by type checkers.
If the duplicate name is a positional-only parameter, no errors should be
generated. For example::

    def foo(name, **kwargs: Unpack[Movie]) -> None: ...     # WRONG! "name" will
                                                            # always bind to the
                                                            # first parameter.

    def foo(name, /, **kwargs: Unpack[Movie]) -> None: ...  # OK! "name" is a
                                                            # positional-only parameter,
                                                            # so **kwargs can contain
                                                            # a "name" keyword.

Required and non-required keys
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

By default all keys in a ``TypedDict`` are required. This behaviour can be
overridden by setting the dictionary's ``total`` parameter as ``False``.
Moreover, :pep:`655` introduced new type qualifiers - ``typing.Required`` and
``typing.NotRequired`` - that enable specifying whether a particular key is
required or not::

    class Movie(TypedDict):
        title: str
        year: NotRequired[int]

When using a ``TypedDict`` to type ``**kwargs`` all of the required and
non-required keys should correspond to required and non-required function
keyword parameters. Therefore, if a required key is not supported by the
caller, then an error must be reported by type checkers.

Assignment
^^^^^^^^^^

Assignments of a function typed with ``**kwargs: Unpack[Movie]`` and
another callable type should pass type checking only if they are compatible.
This can happen for the scenarios described below.

Source and destination contain ``**kwargs``
"""""""""""""""""""""""""""""""""""""""""""

Both destination and source functions have a ``**kwargs: Unpack[TypedDict]``
parameter and the destination function's ``TypedDict`` is assignable to the
source function's ``TypedDict`` and the rest of the parameters are
compatible::

    class Animal(TypedDict):
        name: str
    
    class Dog(Animal):
        breed: str

    def accept_animal(**kwargs: Unpack[Animal]): ...
    def accept_dog(**kwargs: Unpack[Dog]): ...

    accept_dog = accept_animal  # OK! Expression of type Dog can be
                                # assigned to a variable of type Animal.

    accept_animal = accept_dog  # WRONG! Expression of type Animal
                                # cannot be assigned to a variable of type Dog.

.. _PEP 692 assignment dest no kwargs:

Source contains ``**kwargs`` and destination doesn't
""""""""""""""""""""""""""""""""""""""""""""""""""""

The destination callable doesn't contain ``**kwargs``, the source callable
contains ``**kwargs: Unpack[TypedDict]`` and the destination function's keyword
arguments are assignable to the corresponding keys in source function's
``TypedDict``. Moreover, not required keys should correspond to optional
function arguments, whereas required keys should correspond to required
function arguments. Again, the rest of the parameters have to be compatible.
Continuing the previous example::

    class Example(TypedDict):
        animal: Animal 
        string: str
        number: NotRequired[int]
    
    def src(**kwargs: Unpack[Example]): ...
    def dest(*, animal: Dog, string: str, number: int = ...): ...

    dest = src  # OK!

It is worth pointing out that the destination function's parameters that are to
be compatible with the keys and values from the ``TypedDict`` must be keyword
only::

    def dest(dog: Dog, string: str, number: int = ...): ...

    dog: Dog = {"name": "Daisy", "breed": "labrador"}

    dest(dog, "some string")  # OK!

    dest = src                # Type checker error!
    dest(dog, "some string")  # The same call fails at
                              # runtime now because 'src' expects
                              # keyword arguments.

The reverse situation where the destination callable contains
``**kwargs: Unpack[TypedDict]`` and the source callable doesn't contain
``**kwargs`` should be disallowed. This is because, we cannot be sure that
additional keyword arguments are not being passed in when an instance of a
subclass had been assigned to a variable with a base class type and then
unpacked in the destination callable invocation::

    def dest(**kwargs: Unpack[Animal]): ...
    def src(name: str): ...

    dog: Dog = {"name": "Daisy", "breed": "Labrador"}
    animal: Animal = dog

    dest = src      # WRONG!
    dest(**animal)  # Fails at runtime.

Similar situation can happen even without inheritance as compatibility
between ``TypedDict``\s is based on structural subtyping.

Source contains untyped ``**kwargs``
""""""""""""""""""""""""""""""""""""

The destination callable contains ``**kwargs: Unpack[TypedDict]`` and the
source callable contains untyped ``**kwargs``::

    def src(**kwargs): ...
    def dest(**kwargs: Unpack[Movie]): ...

    dest = src  # OK!

Source contains traditionally typed ``**kwargs: T``
"""""""""""""""""""""""""""""""""""""""""""""""""""

The destination callable contains ``**kwargs: Unpack[TypedDict]``, the source
callable contains traditionally typed ``**kwargs: T`` and each of the
destination function ``TypedDict``'s fields is assignable to a variable of
type ``T``::

    class Vehicle:
        ...
    
    class Car(Vehicle):
        ...

    class Motorcycle(Vehicle):
        ...

    class Vehicles(TypedDict):
        car: Car
        moto: Motorcycle
    
    def dest(**kwargs: Unpack[Vehicles]): ...
    def src(**kwargs: Vehicle): ...

    dest = src  # OK!

On the other hand, if the destination callable contains either untyped or
traditionally typed ``**kwargs: T`` and the source callable is typed using
``**kwargs: Unpack[TypedDict]`` then an error should be generated, because
traditionally typed ``**kwargs`` aren't checked for keyword names.

To summarize, function parameters should behave contravariantly and function
return types should behave covariantly.

Passing kwargs inside a function to another function
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

`A previous point <PEP 692 assignment dest no kwargs>`_
mentions the problem of possibly passing additional keyword arguments by
assigning a subclass instance to a variable that has a base class type. Let's
consider the following example::

    class Animal(TypedDict):
        name: str
    
    class Dog(Animal):
        breed: str

    def takes_name(name: str): ...

    dog: Dog = {"name": "Daisy", "breed": "Labrador"}
    animal: Animal = dog

    def foo(**kwargs: Unpack[Animal]):
        print(kwargs["name"].capitalize())
    
    def bar(**kwargs: Unpack[Animal]):
        takes_name(**kwargs)
    
    def baz(animal: Animal):
        takes_name(**animal)
    
    def spam(**kwargs: Unpack[Animal]):
        baz(kwargs)
    
    foo(**animal)   # OK! foo only expects and uses keywords of 'Animal'.

    bar(**animal)   # WRONG! This will fail at runtime because 'breed' keyword
                    # will be passed to 'takes_name' as well.
    
    spam(**animal)  # WRONG! Again, 'breed' keyword will be eventually passed
                    # to 'takes_name'.

In the example above, the call to ``foo`` will not cause any issues at
runtime. Even though ``foo`` expects ``kwargs`` of type ``Animal`` it doesn't
matter if it receives additional arguments because it only reads and uses what
it needs completely ignoring any additional values.

The calls to ``bar`` and ``spam`` will fail because an unexpected keyword
argument will be passed to the ``takes_name`` function.

Therefore, ``kwargs`` hinted with an unpacked ``TypedDict`` can only be passed
to another function if the function to which unpacked kwargs are being passed
to has ``**kwargs`` in its signature as well, because then additional keywords
would not cause errors at runtime during function invocation. Otherwise, the
type checker should generate an error.

In cases similar to the ``bar`` function above the problem could be worked
around by explicitly dereferencing desired fields and using them as arguments
to perform the function call::

    def bar(**kwargs: Unpack[Animal]):
        name = kwargs["name"]
        takes_name(name)

Using ``Unpack`` with types other than ``TypedDict``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

``TypedDict`` is the only permitted heterogeneous type for typing ``**kwargs``.
Therefore, in the context of typing ``**kwargs``, using ``Unpack`` with types
other than ``TypedDict`` should not be allowed and type checkers should
generate errors in such cases.

``@override``
-------------

(Originally specified by :pep:`698`.)

When type checkers encounter a method decorated with ``@typing.override`` they
should treat it as a type error unless that method is overriding a compatible
method or attribute in some ancestor class.


.. code-block:: python

    from typing import override

    class Parent:
        def foo(self) -> int:
            return 1

        def bar(self, x: str) -> str:
            return x

    class Child(Parent):
        @override
        def foo(self) -> int:
            return 2

        @override
        def baz() -> int:  # Type check error: no matching signature in ancestor
            return 1


The ``@override`` decorator should be permitted anywhere a type checker
considers a method to be a valid override, which typically includes not only
normal methods but also ``@property``, ``@staticmethod``, and ``@classmethod``.


Strict Enforcement Per-Project
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We believe that ``@override`` is most useful if checkers also allow developers
to opt into a strict mode where methods that override a parent class are
required to use the decorator. Strict enforcement should be opt-in for backward
compatibility.

``NewType``
-----------

There are also situations where a programmer might want to avoid logical
errors by creating simple classes. For example::

  class UserId(int):
      pass

  def get_by_user_id(user_id: UserId):
      ...

However, this approach introduces a runtime overhead. To avoid this,
``typing.py`` provides a helper function ``NewType`` that creates
simple unique types with almost zero runtime overhead. For a static type
checker ``Derived = NewType('Derived', Base)`` is roughly equivalent
to a definition::

  class Derived(Base):
      def __init__(self, _x: Base) -> None:
          ...

While at runtime, ``NewType('Derived', Base)`` returns a dummy function
that simply returns its argument. Type checkers require explicit casts
from ``int`` where ``UserId`` is expected, while implicitly casting
from ``UserId`` where ``int`` is expected. Examples::

        UserId = NewType('UserId', int)

        def name_by_id(user_id: UserId) -> str:
            ...

        UserId('user')          # Fails type check

        name_by_id(42)          # Fails type check
        name_by_id(UserId(42))  # OK

        num = UserId(5) + 1     # type: int

``NewType`` accepts exactly two arguments: a name for the new unique type,
and a base class. The latter should be a proper class (i.e.,
not a type construct like ``Union``, etc.), or another unique type created
by calling ``NewType``. The function returned by ``NewType``
accepts only one argument; this is equivalent to supporting only one
constructor accepting an instance of the base class (see above). Example::

  class PacketId:
      def __init__(self, major: int, minor: int) -> None:
          self._major = major
          self._minor = minor

  TcpPacketId = NewType('TcpPacketId', PacketId)

  packet = PacketId(100, 100)
  tcp_packet = TcpPacketId(packet)  # OK

  tcp_packet = TcpPacketId(127, 0)  # Fails in type checker and at runtime

Both ``isinstance`` and ``issubclass``, as well as subclassing will fail
for ``NewType('Derived', Base)`` since function objects don't support
these operations.

.. _interaction-with-python:

Interaction with Python features
================================

Special cases for subtyping
---------------------------

Python's numeric types ``complex``, ``float`` and ``int`` are not
subtypes of each other, but to support common use cases, the type
system contains a straightforward shortcut:
when an argument is annotated as having
type ``float``, an argument of type ``int`` is acceptable; similar,
for an argument annotated as having type ``complex``, arguments of
type ``float`` or ``int`` are acceptable.

Arbitrary argument lists and default argument values
----------------------------------------------------

Arbitrary argument lists can as well be type annotated,
so that the definition::

  def foo(*args: str, **kwds: int): ...

is acceptable and it means that, e.g., all of the following
represent function calls with valid types of arguments::

  foo('a', 'b', 'c')
  foo(x=1, y=2)
  foo('', z=0)

In the body of function ``foo``, the type of variable ``args`` is
deduced as ``tuple[str, ...]`` and the type of variable ``kwds``
is ``dict[str, int]``.

In stubs it may be useful to declare an argument as having a default
without specifying the actual default value.  For example::

  def foo(x: AnyStr, y: AnyStr = ...) -> AnyStr: ...

What should the default value look like?  Any of the options ``""``,
``b""`` or ``None`` fails to satisfy the type constraint.

In such cases the default value may be specified as a literal
ellipsis, i.e. the above example is literally what you would write.


Annotating generator functions and coroutines
---------------------------------------------

The return type of generator functions can be annotated by
the generic type ``Generator[yield_type, send_type,
return_type]`` provided by ``typing.py`` module::

  def echo_round() -> Generator[int, float, str]:
      res = yield
      while res:
          res = yield round(res)
      return 'OK'

Coroutines introduced in :pep:`492` are annotated with the same syntax as
ordinary functions. However, the return type annotation corresponds to the
type of ``await`` expression, not to the coroutine type::

  async def spam(ignored: int) -> str:
      return 'spam'

  async def foo() -> None:
      bar = await spam(42)  # type is str

The generic ABC ``collections.abc.Coroutine`` can be used
to specify awaitables that also support
``send()`` and ``throw()`` methods. The variance and order of type variables
correspond to those of ``Generator``, namely ``Coroutine[T_co, T_contra, V_co]``,
for example::

  from collections.abc import Coroutine
  c: Coroutine[list[str], str, int]
  ...
  x = c.send('hi')  # type is list[str]
  async def bar() -> None:
      x = await c  # type is int

The generic ABCs ``Awaitable``,
``AsyncIterable``, and ``AsyncIterator`` can be used for situations where more precise
types cannot be specified::

  def op() -> collections.abc.Awaitable[str]:
      if cond:
          return spam(42)
      else:
          return asyncio.Future(...)
