.. _python-features:

Python features in the type system
----------------------------------

This section describes how certain specific features of the Python
language interact with the type system.

Tuples
~~~~~~

The type of a tuple can be expressed by listing the element
types: ``tuple[int, int, str]`` is a tuple containing an int,
another int, and a str.  The empty tuple can be typed as
``tuple[()]``.  Arbitrary-length homogeneous tuples can be
expressed using one type and ellipsis, for example ``tuple[int, ...]``.
