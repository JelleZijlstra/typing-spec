Callables
=========

Argument defaults
-----------------

It may be useful to declare an argument as having a default
without specifying the actual default value.  For example::

  def foo(x: AnyStr, y: AnyStr = ...) -> AnyStr: ...

What should the default value look like?  Any of the options ``""``,
``b""`` or ``None`` fails to satisfy the type constraint.

In such cases the default value may be specified as a literal
ellipsis, i.e. the above example is literally what you would write.

Annotating ``*args`` and ``**kwargs``
-------------------------------------

Type annotation on variadic positional arguments
(``*args``) and keyword arguments (``**kwargs``) refer to
the types of individual arguments, not to the type of the
entire collection. Therefore, the definition::

  def foo(*args: str, **kwds: int): ...

is acceptable and it means that the function accepts an
arbitrary number of positional arguments of type ``str``
and an arbitrary number of keyword arguments of type ``int``.
For example, all of the following
represent function calls with valid types of arguments::

  foo('a', 'b', 'c')
  foo(x=1, y=2)
  foo('', z=0)

In the body of function ``foo``, the type of variable ``args`` is
deduced as ``tuple[str, ...]`` and the type of variable ``kwds``
is ``dict[str, int]``.
