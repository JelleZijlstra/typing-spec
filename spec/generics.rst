Generics
========

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

A ``TypeVarTuple`` serves as a placeholder not for a single type
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
