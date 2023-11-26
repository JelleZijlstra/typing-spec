Type annotations
================

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
