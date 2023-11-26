Type aliases
============

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
