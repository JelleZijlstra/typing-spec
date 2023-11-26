Class type compatibility
========================

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

Special cases for subtyping
---------------------------

Python's numeric types ``complex``, ``float`` and ``int`` are not
subtypes of each other, but to support common use cases, the type
system contains a straightforward shortcut:
when an argument is annotated as having
type ``float``, an argument of type ``int`` is acceptable; similar,
for an argument annotated as having type ``complex``, arguments of
type ``float`` or ``int`` are acceptable.
