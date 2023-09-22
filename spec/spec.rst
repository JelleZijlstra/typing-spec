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


The meaning of annotations
==========================

Any function without annotations should be treated as having the most
general type possible, or ignored, by any type checker.  Functions
with the ``@no_type_check`` decorator should be treated as having
no annotations.

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


Type Definition Syntax
======================

The syntax leverages :pep:`3107`-style annotations with a number of
extensions described in sections below.  In its basic form, type
hinting is used by filling function annotation slots with classes::

  def greeting(name: str) -> str:
      return 'Hello ' + name

This states that the expected type of the ``name`` argument is
``str``.  Analogically, the expected return type is ``str``.

Expressions whose type is a subtype of a specific argument type are
also accepted for that argument.


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
intentionally somewhat vague requirement, specific inclusions and
exclusions may be added to future versions of this PEP as warranted by
the discussion.)

In addition to the above, the following special constructs defined
below may be used: ``None``, ``Any``, ``Union``, ``Tuple``,
``Callable``, all ABCs and stand-ins for concrete classes exported
from ``typing`` (e.g. ``Sequence`` and ``Dict``), type variables, and
type aliases.


Using None
----------

When used in a type hint, the expression ``None`` is considered
equivalent to ``type(None)``.


Type aliases
------------

(See :pep:`613` for the introduction of ``TypeAlias``.)

Type aliases may be defined by simple variable assignments::

  Url = str

  def retry(url: Url, retry_count: int) -> None: ...

Or by using ``typing.TypeAlias``::

  from typing import TypeAlias

  Url: TypeAlias = str

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
arguments with ``Callable``.  Similarly, there is no support for
specifying callback signatures with a variable number of arguments of a
specific type.

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

In this case the contract is that the returned value is consistent with
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

* A generic class nested in another generic class cannot use same type
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

An upper bound cannot be combined with type constraints (as in used
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
<wiki-variance_>`_ and in :pep:`483`; here we just show how to control
a type checker's behavior.

By default generic types are considered *invariant* in all type variables,
which means that values for variables annotated with types like
``list[Employee]`` must exactly match the type annotation -- no subclasses or
superclasses of the type parameter (in this example ``Employee``) are
allowed.

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


Special cases for subtyping
---------------------------

Python's numeric types ``complex``, ``float`` and ``int`` are not
subtypes of each other, but to support common use cases, the type
system contains a straightforward shortcut:
when an argument is annotated as having
type ``float``, an argument of type ``int`` is acceptable; similar,
for an argument annotated as having type ``complex``, arguments of
type ``float`` or ``int`` are acceptable.


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


Runtime or type checking?
-------------------------

Sometimes there's code that must be seen by a type checker (or other
static analysis tools) but should not be executed.  For such
situations the ``typing`` module defines a constant,
``TYPE_CHECKING``, that is considered ``True`` during type checking
(or other static analysis) but ``False`` at runtime.  Example::

  import typing

  if typing.TYPE_CHECKING:
      import expensive_mod

  def a_func(arg: 'expensive_mod.SomeClass') -> None:
      a_var: expensive_mod.SomeClass = arg
      ...

(Note that the type annotation must be enclosed in quotes, making it a
"forward reference", to hide the ``expensive_mod`` reference from the
interpreter runtime.  In the variable annotation no quotes are needed.)

This approach may also be useful to handle import cycles.


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
in ``__init__`` or ``__new__``. The proposed syntax is as follows::

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

We propose to use the term *protocols* for types supporting structural
subtyping. The reason is that the term *iterator protocol*,
for example, is widely understood in the community, and coming up with
a new term for this concept in a statically typed context would just create
confusion.

This has the drawback that the term *protocol* becomes overloaded with
two subtly different meanings: the first is the traditional, well-known but
slightly fuzzy concept of protocols such as iterator; the second is the more
explicitly defined concept of protocols in statically typed code.
The distinction is not important most of the time, and in other
cases we propose to just add a qualifier such as *protocol classes*
when referring to the static type concept.

If a class includes a protocol in its MRO, the class is called
an *explicit* subclass of the protocol. If a class is a structural subtype
of a protocol, it is said to implement the protocol and to be compatible
with a protocol. If a class is compatible with a protocol but the protocol
is not included in the MRO, the class is an *implicit* subtype
of the protocol. (Note that one can explicitly subclass a protocol and
still not implement it if a protocol attribute is set to ``None``
in the subclass, see Python [data-model]_ for details.)

The attributes (variables and methods) of a protocol that are mandatory
for other class in order to be considered a structural subtype are called
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

Apart from few restrictions explicitly mentioned below, protocol types can
be used in every context where a normal types can::

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
subtypes, the main benefit of explicit subclassing is to get some protocol
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
is not changed, all of them must be implemented by an explicit subclass
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
instantiation of parameters with such type. For example::

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

* *Data, and non-data protocols*: A protocol is called non-data protocol
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
it’s safe to do things like ``foo + 5`` since ``foo`` inherits int’s
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
However, ``Literal[0]`` and ``Literal[False]`` is *not* equivalent
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
allowing them in future extensions of this PEP.

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
with more sophisticated inference techniques. This PEP does not mandate any
particular strategy; it only emphasizes the importance of backwards compatibility.

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

**Note:** See `Interactions with Final`_ for a proposal on how we can
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

A little more broadly: we propose adding a policy to typeshed that
mandates that whenever we add literal types to some existing API, we also
always include a fallback overload to maintain backwards-compatibility.

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
almost certainly likely find Literal types as proposed in this PEP to be
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

This PEP proposes two ways of defining TypedDict types.  The first uses
a class-based syntax.  The second is an alternative
assignment-based syntax that is provided for backwards compatibility,
to allow the feature to be backported to older Python versions.  The
rationale is similar to why :pep:`484` supports a comment-based
annotation syntax for Python 2.7: type hinting is particularly useful
for large existing codebases, and these often need to run on older
Python versions.  The two syntax options parallel the syntax variants
supported by ``typing.NamedTuple``.  Other proposed features include
TypedDict inheritance and totality (specifying whether keys are
required or not).

This PEP also provides a sketch of how a type checker is expected
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
  syntax in Python 2.7.)  Instead, this PEP provides an alternative,
  assignment-based syntax for backwards compatibility, discussed in
  `Alternative Syntax`_.

* String literal forward references are valid in the value types.

* Methods are not allowed, since the runtime type of a TypedDict
  object will always be just ``dict`` (it is never a subclass of
  ``dict``).

* Specifying a metaclass is not allowed.

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
non-TypedDict base class.

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

This PEP also proposes an alternative syntax that can be backported to
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
annotations, which may always want ``Annotated[]`` as the outermost annotation.
[3]_


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

Type variable tuples are created with:

::

    from typing import TypeVarTuple

    Ts = TypeVarTuple('Ts')

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

To keep this PEP minimal, ``TypeVarTuple`` does not yet support specification of:

* Variance (e.g. ``TypeVar('T', covariant=True)``)
* Type constraints (``TypeVar('T', int, float)``)
* Type bounds (``TypeVar('T', bound=ParentClass)``)

We leave the decision of how these arguments should behave to a future PEP, when variadic generics have been tested in the field. As of this PEP, type variable tuples are
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

As of this PEP, only a single type variable tuple may appear in a type parameter list:

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

This PEP introduces the symbol ``TypeGuard`` exported from the ``typing``
module. ``TypeGuard`` is a special form that accepts a single type argument.
It is used to annotate the return type of a user-defined type guard function.
Return statements within a type guard function should return bool values,
and type checkers should verify that all return paths return a bool.

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

Compatibility with other uses of function annotations
=====================================================

A number of existing or potential use cases for function annotations
exist, which are incompatible with type hinting.  These may confuse
a static type checker.  However, since type hinting annotations have no
runtime behavior (other than evaluation of the annotation expression and
storing annotations in the ``__annotations__`` attribute of the function
object), this does not make the program incorrect -- it just may cause
a type checker to emit spurious warnings or errors.

To mark portions of the program that should not be covered by type
hinting, you can use one or more of the following:

* a ``# type: ignore`` comment;

* a ``@no_type_check`` decorator on a class or function;

* a custom class or function decorator marked with
  ``@no_type_check_decorator``.

For more details see later sections.


``# type: ignore`` comments
---------------------------

The special comment ``# type: ignore`` is used to silence type checker
errors.

The ``# type: ignore`` comment should be put on the line that the
error refers to::

  import http.client
  errors = {
      'not_found': http.client.NOT_FOUND  # type: ignore
  }

A ``# type: ignore`` comment on a line by itself at the top of a file,
before any docstrings, imports, or other executable code, silences all
errors in the file. Blank lines and other comments, such as shebang
lines and coding cookies, may precede the ``# type: ignore`` comment.

In some cases, linting tools or other comments may be needed on the same
line as a type comment. In these cases, the type comment should be before
other comments and linting markers:

  # type: ignore # <comment or other marker>

If type hinting proves useful in general, a syntax for typing variables
may be provided in a future Python version. (**UPDATE**: This syntax
was added in Python 3.6 through :pep:`526`.)

Casts
=====

Occasionally the type checker may need a different kind of hint: the
programmer may know that an expression is of a more constrained type
than a type checker may be able to infer.  For example::

  from typing import cast

  def find_first_str(a: list[object]) -> str:
      index = next(i for i, x in enumerate(a) if isinstance(x, str))
      # We only get here if there's at least one string in a
      return cast(str, a[index])

Some type checkers may not be able to infer that the type of
``a[index]`` is ``str`` and only infer ``object`` or ``Any``, but we
know that (if the code gets to that point) it must be a string.  The
``cast(t, x)`` call tells the type checker that we are confident that
the type of ``x`` is ``t``.  At runtime a cast always returns the
expression unchanged -- it does not check the type, and it does not
convert or coerce the value.

Casts differ from type comments (see the previous section).  When using
a type comment, the type checker should still verify that the inferred
type is consistent with the stated type.  When using a cast, the type
checker should blindly believe the programmer.  Also, casts can be used
in expressions, while type comments only apply to assignments.


NewType helper function
=======================

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


Stub Files
==========

Stub files are files containing type hints that are only for use by
the type checker, not at runtime.  There are several use cases for
stub files:

* Extension modules

* Third-party modules whose authors have not yet added type hints

* Standard library modules for which type hints have not yet been
  written

* Modules that must be compatible with Python 2 and 3

* Modules that use annotations for other purposes

Stub files have the same syntax as regular Python modules.  There is one
feature of the ``typing`` module that is different in stub files:
the ``@overload`` decorator described below.

The type checker should only check function signatures in stub files;
It is recommended that function bodies in stub files just be a single
ellipsis (``...``).

The type checker should have a configurable search path for stub files.
If a stub file is found the type checker should not read the
corresponding "real" module.

While stub files are syntactically valid Python modules, they use the
``.pyi`` extension to make it possible to maintain stub files in the
same directory as the corresponding real module.  This also reinforces
the notion that no runtime behavior should be expected of stub files.

Additional notes on stub files:

* Modules and variables imported into the stub are not considered
  exported from the stub unless the import uses the ``import ... as
  ...`` form or the equivalent ``from ... import ... as ...`` form.
  (*UPDATE:* To clarify, the intention here is that only names
  imported using the form ``X as X`` will be exported, i.e. the name
  before and after ``as`` must be the same.)

* However, as an exception to the previous bullet, all objects
  imported into a stub using ``from ... import *`` are considered
  exported.  (This makes it easier to re-export all objects from a
  given module that may vary by Python version.)

* Just like in `normal Python files <importdocs_>`_, submodules
  automatically become exported attributes of their parent module
  when imported. For example, if the ``spam`` package has the
  following directory structure::

      spam/
          __init__.pyi
          ham.pyi

  where ``__init__.pyi`` contains a line such as ``from . import ham``
  or ``from .ham import Ham``, then ``ham`` is an exported attribute
  of ``spam``.

* Stub files may be incomplete. To make type checkers aware of this, the file
  can contain the following code::

    def __getattr__(name) -> Any: ...

  Any identifier not defined in the stub is therefore assumed to be of type
  ``Any``.

The Typeshed Repo
-----------------

There is a `shared repository <typeshed_>`_ where useful stubs are being
collected.  Policies regarding the stubs collected here are
decided separately and reported in the repo's documentation.


Storing and distributing types for library packages
====================================================

There are several motivations and methods of supporting typing in a package.
This specification recognizes three types of packages that users of typing wish to
create:

1. The package maintainer would like to add type information inline.

2. The package maintainer would like to add type information via stubs.

3. A third party or package maintainer would like to share stub files for
   a package, but the maintainer does not want to include them in the source
   of the package.

This specification aims to support all three scenarios and make them simple to add to
packaging and deployment.

The two major parts of this specification are the packaging specifications
and the resolution order for resolving module type information.


Packaging Type Information
--------------------------

In order to make packaging and distributing type information as simple and
easy as possible, packaging and distribution is done through existing
frameworks.

Package maintainers who wish to support type checking of their code MUST add
a marker file named ``py.typed`` to their package supporting typing. This marker applies
recursively: if a top-level package includes it, all its sub-packages MUST support
type checking as well. To have this file installed with the package,
maintainers can use existing packaging options such as ``package_data`` in
distutils, shown below.

Distutils option example::

    setup(
        ...,
        package_data = {
            'foopkg': ['py.typed'],
        },
        ...,
        )

For namespace packages (see :pep:`420`), the ``py.typed`` file should be in the
submodules of the namespace, to avoid conflicts and for clarity.

This specification does not support distributing typing information as part of
module-only distributions or single-file modules within namespace packages.

The single-file module should be refactored into a package
and indicate that the package supports typing as described
above.

Stub-only Packages
^^^^^^^^^^^^^^^^^^

For package maintainers wishing to ship stub files containing all of their
type information, it is preferred that the ``*.pyi`` stubs are alongside the
corresponding ``*.py`` files. However, the stubs can also be put in a separate
package and distributed separately. Third parties can also find this method
useful if they wish to distribute stub files. The name of the stub package
MUST follow the scheme ``foopkg-stubs`` for type stubs for the package named
``foopkg``. Note that for stub-only packages adding a ``py.typed`` marker is not
needed since the name ``*-stubs`` is enough to indicate it is a source of typing
information.

Third parties seeking to distribute stub files are encouraged to contact the
maintainer of the package about distribution alongside the package. If the
maintainer does not wish to maintain or package stub files or type information
inline, then a third party stub-only package can be created.

In addition, stub-only distributions SHOULD indicate which version(s)
of the runtime package are supported by indicating the runtime distribution's
version(s) through normal dependency data. For example, the
stub package ``flyingcircus-stubs`` can indicate the versions of the
runtime ``flyingcircus`` distribution it supports through ``install_requires``
in distutils-based tools, or the equivalent in other packaging tools. Note that
in pip 9.0, if you update ``flyingcircus-stubs``, it will update
``flyingcircus``. In pip 9.0, you can use the
``--upgrade-strategy=only-if-needed`` flag. In pip 10.0 this is the default
behavior.

For namespace packages (see :pep:`420`), stub-only packages should
use the ``-stubs`` suffix on only the root namespace package.
All stub-only namespace packages should omit ``__init__.pyi`` files. ``py.typed``
marker files are not necessary for stub-only packages, but similarly
to packages with inline types, if used, they should be in submodules of the namespace to
avoid conflicts and for clarity.

For example, if the ``pentagon`` and ``hexagon`` are separate distributions
installing within the namespace package ``shapes.polygons``
The corresponding types-only distributions should produce packages
laid out as follows::

    shapes-stubs
    └── polygons
        └── pentagon
            └── __init__.pyi

    shapes-stubs
    └── polygons
        └── hexagon
            └── __init__.pyi

.. _mro:

Type Checker Module Resolution Order
------------------------------------

The following is the order in which type checkers supporting this specification SHOULD
resolve modules containing type information:


1. Stubs or Python source manually put in the beginning of the path. Type
   checkers SHOULD provide this to allow the user complete control of which
   stubs to use, and to patch broken stubs/inline types from packages.
   In mypy the ``$MYPYPATH`` environment variable can be used for this.

2. User code - the files the type checker is running on.

3. Stub packages - these packages SHOULD supersede any installed inline
   package. They can be found at ``foopkg-stubs`` for package ``foopkg``.

4. Packages with a ``py.typed`` marker file - if there is nothing overriding
   the installed package, *and* it opts into type checking, the types
   bundled with the package SHOULD be used (be they in ``.pyi`` type
   stub files or inline in ``.py`` files).

5. Typeshed (if used) - Provides the stdlib types and several third party
   libraries.

If typecheckers identify a stub-only namespace package without the desired module
in step 3, they should continue to step 4/5. Typecheckers should identify namespace packages
by the absence of ``__init__.pyi``.  This allows different subpackages to
independently opt for inline vs stub-only.

Type checkers that check a different Python version than the version they run
on MUST find the type information in the ``site-packages``/``dist-packages``
of that Python version. This can be queried e.g.
``pythonX.Y -c 'import site; print(site.getsitepackages())'``. It is also recommended
that the type checker allow for the user to point to a particular Python
binary, in case it is not in the path.


Partial Stub Packages
---------------------

Many stub packages will only have part of the type interface for libraries
completed, especially initially. For the benefit of type checking and code
editors, packages can be "partial". This means modules not found in the stub
package SHOULD be searched for in parts four and five of the module resolution
order above, namely inline packages and typeshed.

Type checkers should merge the stub package and runtime package or typeshed
directories. This can be thought of as the functional equivalent of copying the
stub package into the same directory as the corresponding runtime package or
typeshed folder and type checking the combined directory structure. Thus type
checkers MUST maintain the normal resolution order of checking ``*.pyi`` before
``*.py`` files.

If a stub package distribution is partial it MUST include ``partial\n`` in a
``py.typed`` file.  For stub-packages distributing within a namespace
package (see :pep:`420`), the ``py.typed`` file should be in the
submodules of the namespace.

Type checkers should treat namespace packages within stub-packages as
incomplete since multiple distributions may populate them.
Regular packages within namespace packages in stub-package distributions
are considered complete unless a ``py.typed`` with ``partial\n`` is included.


The ``typing`` Module
=====================

To open the usage of static type checking to Python 3.5 as well as older
versions, a uniform namespace is required.  For this purpose, the
standard library contains the ``typing`` module.

It defines the fundamental building blocks for constructing types
(e.g. ``Any``), types representing generic variants of builtin
collections (e.g. ``List``), types representing generic
collection ABCs (e.g. ``Sequence``), and a small collection of
convenience definitions.

Note that special type constructs, such as ``Any``,
and type variables defined using ``TypeVar`` are only supported
in the type annotation context, and ``Generic`` may only be used
as a base class. All of these (except for unparameterized generics)
will raise ``TypeError`` if appear in ``isinstance`` or ``issubclass``.

Fundamental building blocks:

* Any, used as ``def get(key: str) -> Any: ...``

* Union, used as ``Union[Type1, Type2, Type3]``

* Callable, used as ``Callable[[Arg1Type, Arg2Type], ReturnType]``

* Tuple, used by listing the element types, for example
  ``Tuple[int, int, str]``.
  The empty tuple can be typed as ``Tuple[()]``.
  Arbitrary-length homogeneous tuples can be expressed
  using one type and ellipsis, for example ``Tuple[int, ...]``.
  (The ``...`` here are part of the syntax, a literal ellipsis.)

* TypeVar, used as ``X = TypeVar('X', Type1, Type2, Type3)`` or simply
  ``Y = TypeVar('Y')`` (see above for more details)

* Generic, used to create user-defined generic classes

* Type, used to annotate class objects

Generic variants of builtin collections:

* Dict, used as ``Dict[key_type, value_type]``

* DefaultDict, used as ``DefaultDict[key_type, value_type]``,
  a generic variant of ``collections.defaultdict``

* List, used as ``List[element_type]``

* Set, used as ``Set[element_type]``. See remark for ``AbstractSet``
  below.

* FrozenSet, used as ``FrozenSet[element_type]``

Note: ``Dict``, ``DefaultDict``, ``List``, ``Set`` and ``FrozenSet``
are mainly useful for annotating return values.
For arguments, prefer the abstract collection types defined below,
e.g.  ``Mapping``, ``Sequence`` or ``AbstractSet``.

Generic variants of container ABCs (and a few non-containers):

* Awaitable

* AsyncIterable

* AsyncIterator

* ByteString

* Callable (see above, listed here for completeness)

* Collection

* Container

* ContextManager

* Coroutine

* Generator, used as ``Generator[yield_type, send_type,
  return_type]``.  This represents the return value of generator
  functions.  It is a subtype of ``Iterable`` and it has additional
  type variables for the type accepted by the ``send()`` method (it
  is contravariant in this variable -- a generator that accepts sending it
  ``Employee`` instance is valid in a context where a generator is required
  that accepts sending it ``Manager`` instances) and the return type of the
  generator.

* Hashable (not generic, but present for completeness)

* ItemsView

* Iterable

* Iterator

* KeysView

* Mapping

* MappingView

* MutableMapping

* MutableSequence

* MutableSet

* Sequence

* Set, renamed to ``AbstractSet``. This name change was required
  because ``Set`` in the ``typing`` module means ``set()`` with
  generics.

* Sized (not generic, but present for completeness)

* ValuesView

A few one-off types are defined that test for single special methods
(similar to ``Hashable`` or ``Sized``):

* Reversible, to test for ``__reversed__``

* SupportsAbs, to test for ``__abs__``

* SupportsComplex, to test for ``__complex__``

* SupportsFloat, to test for ``__float__``

* SupportsInt, to test for ``__int__``

* SupportsRound, to test for ``__round__``

* SupportsBytes, to test for ``__bytes__``

Convenience definitions:

* Optional, defined by ``Optional[t] == t | None``

* Text, a simple alias for ``str`` in Python 3, for ``unicode`` in Python 2

* AnyStr, defined as ``TypeVar('AnyStr', Text, bytes)``

* NamedTuple, used as
  ``NamedTuple(type_name, [(field_name, field_type), ...])``
  and equivalent to
  ``collections.namedtuple(type_name, [field_name, ...])``.
  This is useful to declare the types of the fields of a named tuple
  type.

* NewType, used to create unique types with little runtime overhead
  ``UserId = NewType('UserId', int)``

* cast(), described earlier

* @no_type_check, a decorator to disable type checking per class or
  function (see below)

* @no_type_check_decorator, a decorator to create your own decorators
  with the same meaning as ``@no_type_check`` (see below)

* @type_check_only, a decorator only available during type checking
  for use in stub files (see above); marks a class or function as
  unavailable during runtime

* @overload, described earlier

* get_type_hints(), a utility function to retrieve the type hints from a
  function or method.  Given a function or method object, it returns
  a dict with the same format as ``__annotations__``, but evaluating
  forward references (which are given as string literals) as expressions
  in the context of the original function or method definition.

* TYPE_CHECKING, ``False`` at runtime but ``True`` to  type checkers

I/O related types:

* IO (generic over ``AnyStr``)

* BinaryIO (a simple subtype of ``IO[bytes]``)

* TextIO (a simple subtype of ``IO[str]``)

Types related to regular expressions and the ``re`` module:

* Match and Pattern, types of ``re.match()`` and ``re.compile()``
  results (generic over ``AnyStr``)

Definition of Terms
===================

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


Syntax alternatives
===================

Over the course of the development of the Python type system, several
changes were made to the Python grammar and standard library to make it
easier to use the type system. This document aims to use the more
modern syntax in all examples, but type checkers should generally support
the older alternatives and treat them as equivalent.

This section lists all of these cases.

.. TODO: add 604 (unions), 646 (Unpack), 695 (type parameters)

Type comments
-------------

No first-class syntax support for explicitly marking variables as being
of a specific type existed when the type system was first designed.
To help with type inference in
complex cases, a comment of the following format may be used::

  x = []                # type: list[Employee]
  x, y, z = [], [], []  # type: list[int], list[int], list[str]
  x, y, z = [], [], []  # type: (list[int], list[int], list[str])
  a, b, *c = range(5)   # type: float, float, list[float]
  x = [1, 2]            # type: list[int]

Type comments should be put on the last line of the statement that
contains the variable definition. 

These should be treated as equivalent to annotating the variables
using :pep:`526` variable annotations::

  x: list[Employee] = []
  x: list[int]
  y: list[int]
  z: list[str]
  x, y, z = [], [], []
  a: float
  b: float
  c: list[float]
  a, b, *c = range(5)
  x: list[int] = [1, 2]

Type comments can also be placed on
``with`` statements and ``for`` statements, right after the colon.

Examples of type comments on ``with`` and ``for`` statements::

  with frobnicate() as foo:  # type: int
      # Here foo is an int
      ...

  for x, y in points:  # type: float, float
      # Here x and y are floats
      ...

In stubs it may be useful to declare the existence of a variable
without giving it an initial value.  This can be done using :pep:`526`
variable annotation syntax::

  from typing import IO

  stream: IO[str]

The above syntax is acceptable in stubs for all versions of Python.
However, in non-stub code for versions of Python 3.5 and earlier
there is a special case::

  from typing import IO

  stream = None  # type: IO[str]

Type checkers should not complain about this (despite the value
``None`` not matching the given type), nor should they change the
inferred type to ``... | None``.  The
assumption here is that other code will ensure that the variable is
given a value of the proper type, and all uses can assume that the
variable has the given type.

Type comments on function definitions
-------------------------------------

Some tools may want to support type annotations in code that must be
compatible with Python 2.7.  For this purpose this PEP has a suggested
(but not mandatory) extension where function annotations are placed in
a ``# type:`` comment.  Such a comment must be placed immediately
following the function header (before the docstring).  An example: the
following Python 3 code::

  def embezzle(self, account: str, funds: int = 1000000, *fake_receipts: str) -> None:
      """Embezzle funds from account using fake receipts."""
      <code goes here>

is equivalent to the following::

  def embezzle(self, account, funds=1000000, *fake_receipts):
      # type: (str, int, *str) -> None
      """Embezzle funds from account using fake receipts."""
      <code goes here>

Note that for methods, no type is needed for ``self``.

For an argument-less method it would look like this::

  def load_cache(self):
      # type: () -> bool
      <code>

Sometimes you want to specify the return type for a function or method
without (yet) specifying the argument types.  To support this
explicitly, the argument list may be replaced with an ellipsis.
Example::

  def send_email(address, sender, cc, bcc, subject, body):
      # type: (...) -> bool
      """Send an email message.  Return True if successful."""
      <code>

Sometimes you have a long list of parameters and specifying their
types in a single ``# type:`` comment would be awkward.  To this end
you may list the arguments one per line and add a ``# type:`` comment
per line after an argument's associated comma, if any.
To specify the return type use the ellipsis syntax. Specifying the return
type is not mandatory and not every argument needs to be given a type.
A line with a ``# type:`` comment should contain exactly one argument.
The type comment for the last argument (if any) should precede the close
parenthesis. Example::

  def send_email(address,     # type: Union[str, List[str]]
                 sender,      # type: str
                 cc,          # type: Optional[List[str]]
                 bcc,         # type: Optional[List[str]]
                 subject='',
                 body=None    # type: List[str]
                 ):
      # type: (...) -> bool
      """Send an email message.  Return True if successful."""
      <code>

Notes:

- Tools that support this syntax should support it regardless of the
  Python version being checked.  This is necessary in order to support
  code that straddles Python 2 and Python 3.

- It is not allowed for an argument or return value to have both
  a type annotation and a type comment.

- When using the short form (e.g. ``# type: (str, int) -> None``)
  every argument must be accounted for, except the first argument of
  instance and class methods (those are usually omitted, but it's
  allowed to include them).

- The return type is mandatory for the short form.  If in Python 3 you
  would omit some argument or the return type, the Python 2 notation
  should use ``Any``.

- When using the short form, for ``*args`` and ``**kwds``, put 1 or 2
  stars in front of the corresponding type annotation.  (As with
  Python 3 annotations, the annotation here denotes the type of the
  individual argument values, not of the tuple/dict that you receive
  as the special argument value ``args`` or ``kwds``.)

- Like other type comments, any names used in the annotations must be
  imported or defined by the module containing the annotation.

- When using the short form, the entire annotation must be one line.

- The short form may also occur on the same line as the close
  parenthesis, e.g.::

    def add(a, b):  # type: (int, int) -> int
        return a + b

- Misplaced type comments will be flagged as errors by a type checker.
  If necessary, such comments could be commented twice. For example::

    def f():
        '''Docstring'''
        # type: () -> None  # Error!

    def g():
        '''Docstring'''
        # # type: () -> None  # This is OK

When checking Python 2.7 code, type checkers should treat the ``int`` and
``long`` types as equivalent. For parameters typed as ``Text``, arguments of
type ``str`` as well as ``unicode`` should be acceptable.


Positional-only arguments
-------------------------

Some functions are designed to take their arguments only positionally,
and expect their callers never to use the argument's name to provide
that argument by keyword. Before Python 3.8 (:pep:`570`), Python did
not provide a way to declare positional-only arguments.

To support positional-only arguments on older Python versions, type
checkers support the following special case:
all arguments with names beginning with
``__`` are assumed to be positional-only, except if their names also
end with ``__``::

  def quux(__x: int, __y__: int = 0) -> None: ...

  quux(3, __y__=1)  # This call is fine.

  quux(__x=3)  # This call is an error.


Generics in standard collections
--------------------------------

Before Python 3.9 (:pep:`585`), standard library generic types like
``list`` could not be parameterized at runtime (i.e., ``list[int]``
would throw an error). Therefore, the ``typing`` module provided
generic aliases for major builtin and standard library types (e.g.,
``typing.List[int]``).

In each of these cases, type checkers should treat the library type
as equivalent to the alias in the ``typing`` module. This includes:

* ``list`` and ``typing.List``
* ``dict`` and ``typing.Dict``
* ``set`` and ``typing.Set``
* ``frozenset`` and ``typing.FrozenSet``
* ``tuple`` and ``typing.Tuple``
* ``type`` and ``typing.Type``
* ``collections.deque`` and ``typing.Deque``
* ``collections.defaultdict`` and ``typing.DefaultDict``
* ``collections.OrderedDict`` and ``typing.OrderedDict``
* ``collections.Counter`` and ``typing.Counter``
* ``collections.ChainMap`` and ``typing.ChainMap``
* ``collections.abc.Awaitable`` and ``typing.Awaitable``
* ``collections.abc.Coroutine`` and ``typing.Coroutine``
* ``collections.abc.AsyncIterable`` and ``typing.AsyncIterable``
* ``collections.abc.AsyncIterator`` and ``typing.AsyncIterator``
* ``collections.abc.AsyncGenerator`` and ``typing.AsyncGenerator``
* ``collections.abc.Iterable`` and ``typing.Iterable``
* ``collections.abc.Iterator`` and ``typing.Iterator``
* ``collections.abc.Generator`` and ``typing.Generator``
* ``collections.abc.Reversible`` and ``typing.Reversible``
* ``collections.abc.Container`` and ``typing.Container``
* ``collections.abc.Collection`` and ``typing.Collection``
* ``collections.abc.Callable`` and ``typing.Callable``
* ``collections.abc.Set`` and ``typing.AbstractSet`` (note the change in name)
* ``collections.abc.MutableSet`` and ``typing.MutableSet``
* ``collections.abc.Mapping`` and ``typing.Mapping``
* ``collections.abc.MutableMapping`` and ``typing.MutableMapping``
* ``collections.abc.Sequence`` and ``typing.Sequence``
* ``collections.abc.MutableSequence`` and ``typing.MutableSequence``
* ``collections.abc.ByteString`` and ``typing.ByteString``
* ``collections.abc.MappingView`` and ``typing.MappingView``
* ``collections.abc.KeysView`` and ``typing.KeysView``
* ``collections.abc.ItemsView`` and ``typing.ItemsView``
* ``collections.abc.ValuesView`` and ``typing.ValuesView``
* ``contextlib.AbstractContextManager`` and ``typing.ContextManager`` (note the change in name)
* ``contextlib.AbstractAsyncContextManager`` and ``typing.AsyncContextManager`` (note the change in name)
* ``re.Pattern`` and ``typing.Pattern``
* ``re.Match`` and ``typing.Match``

The generic aliases in the ``typing`` module are considered deprecated
and type checkers may warn if they are used.

``Union`` and ``Optional``
--------------------------

Before Python 3.10 (:pep:`604`), Python did not support the ``|`` operator
for creating unions of types. Therefore, the ``typing.Union`` special form can also
be used to create union types. Type checkers should treat the two forms as equivalent.

In addition, the ``Optional`` special form provides a shortcut for a union with ``None``.

Examples:

* ``int | str`` is the same as ``Union[int, str]``
* ``int | str | range`` is the same as ``Union[int, str, range]``
* ``int | None`` is the same as ``Optional[int]`` and ``Union[int, None]``

``Unpack``
----------

:pep:`646`, which introduced ``TypeVarTuple`` into Python 3.11, also made two grammar
changes to support use of variadic generics, allowing use of the ``*`` operator in
index operations and in ``*args`` annotations. The ``Unpack[]`` operator was added to
support equivalent semantics on older Python versions. It should be treated as equivalent
to the ``*`` syntax. In particular, the following are equivalent:

* ``A[*Ts]`` is the same as ``A[Unpack[Ts]]``
* ``def f(*args: *Ts): ...`` is the same as ``def f(*args: Unpack[Ts]): ...``

.. _mypy:
   http://mypy-lang.org

.. _gvr-artima:
   https://www.artima.com/weblogs/viewpost.jsp?thread=85551

.. _wiki-variance:
   https://en.wikipedia.org/wiki/Covariance_and_contravariance_%28computer_science%29

.. _typeshed:
   https://github.com/python/typeshed

.. _roberge:
   https://aroberge.blogspot.com/2015/01/type-hinting-in-python-focus-on.html

.. _github:
   https://github.com/python/typing

.. _issues:
   https://github.com/python/typing/issues

.. _peps:
   https://hg.python.org/peps/file/tip/pep-0484.txt

.. _importdocs:
   https://docs.python.org/3/reference/import.html#submodules

.. [data-model]
   https://docs.python.org/3/reference/datamodel.html#special-method-names

.. [3] https://bugs.python.org/issue46491
