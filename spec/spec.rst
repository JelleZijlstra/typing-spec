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
