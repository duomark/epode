epode
=====

Epode (Erlang Persistable Object Dictionary Entities) is a library which provides multiple implementations of a dictionary interface to a bundle of properties which can represent an "object".  Internally, database dictionaries are implemented in a format that is convenient and comparable to the resting format in a database, yet allows one of multiple external databases to be used (initially, redis and dets, with a mecked interface to the process dictionary for scoped testing). The dictionaries may also be converted between the database-style representation and an application-style representation so that they may be embedded as data instances within a larger abstract application without polluting the datatype space of the application.

This library is released under a "Modified BSD License". The library was sponsored by [TigerText](http://tigertext.com/) during 2014, and was validated on their HIPAA-compliant secure text XMPP servers in a production environment handling more than 1M messages per day.

Genesis
-------

At TigerText we store data objects (users, messages, groups, organizations) in redis, but were considering using another disk-based database to store some collections that were exceeding memory and/or had archival requirements that couldn't be met by redis. The keys and values in redis were implemented in erlang by the library we chose as binary attributes and binary values, but some of the values really represented lists, booleans and other datatypes. Likewise, the attribute names in redis were not necessarily the attribute names we used in our application code. To allow flexibility for testing and comparing different databases, yet still retain a single object interface from the application, we decided to wrap a dictionary behaviour allowing multiple internal implementations with a consistent API so that components could be replaced and benchmarked within a running production system.

The approach uses layering of abstractions to mask the implementation of lower levels (the list below works its way from the database layer to the application layer):

  1. Database implementation (can be key/value, column store, relational, or any other form)
  2. Library to bring database data into the erlang world
  3. Epode_Db dictionaries (keys and values are both binaries)
  4. Epode_App dictionaries (keys are atoms, values are integrator-selected erlang datatypes)

Epode_Db and Epode_App datatypes are tagged dictionaries {Type::atom(), Dict::epode_dict()} with multiple implementations of epode_dict() available:

  1. proplists
  2. erlang:dict
  3. erlang:orddict
  4. bindict (a single binary representing a binary-based orddict)
  5. erlang maps (in future, requires 17.0 or later)

The [epode name] (https://www.google.com/search?q=define%3Aepode) refers to the fact that there are 2 versions of any object normally available: a core smaller object and the full object containing all available properties. The core is presumably smaller and faster to retrieve from a database, thereby placing less memory and latency pressure on an application when it needs to quickly display a list of available data items or provide just the basic definitions or a collection of objects. If more functionality is needed by the application, a full object can be retrieved.

Philosophy
----------

The strategy behind epode is to write an application using object-like abstractions with the flexibility to change the core internal data structures or backing data store. The following design principles underly the library:

  1. Isolation
  2. Separation of Concerns
  3. Object Behavior Abstraction

Isolation of elements is provided by layering data access. Interactions with the external data store only occur in the bottom layer which brings binaries into the Erlang VM, validation and data conversion occur only in the translation layer, and application-level interaction operates only on clean, verified datatypes. This allows the use of extensive property-based testing using PropEr since the layers can be tested independently.

The separation of concerns means that the datatypes and data values of any application layer can be modified without impacting the other layers. This allows testing and tuning of performance without having changes cause impact in other areas of the application.

Object behaviors are captured in the APIs available at each layer of the library. By using erlang behaviours at the lowest levels, it is possible to use meck to replace real elements during testing for more control, and to swap one implementation for another, for example when changing to a different backing store. The rest of the application will interact with Epode_App objects. These should be encapsulated with your own API which expresses higher-level abstract behavior of an object so that you can change the dictionary implementation or attributes without unnecessarily impacting the application logic.

Epode Operations
----------------

The choice of a dictionary to implement epodes was made to support a more flexible object definition. Since we started with a Key/Value store, there is no consistent schema required for our objects (although all of them share a fixed set of primary properties). A dictionary also provides the following benefits:

  1. Subsets or components of an object are equivalently represented
  2. Conversions of tagged dictionaries are concisely and efficiently coded
  3. Behaviours can expose a consistent interface while the underlying implementation changes
  4. Dictionary interfaces are commonly encountered by implementers even in other languages
  5. Test suites can simply meck the interface to an ephemeral process dictionary
  6. Tagged dictionaries are typed and self-describing
  7. One benefit of #6 is ease in transmitting objects to other nodes
  8. Support for contextual shadowing of property values or full dictionaries
  9. Functional composition is orthogonal to the dictionary, not bound in its implementation

The use of projection and property vectors allows quick, functional destructuring of a collection of objects. This is useful when sorting, coordinating related properties or imposing/verifying that cross-property constraints are satisfied. Data conversion can also be applied using projections, so that the amount of work needed to produce the converted collection of properties is optimized. The use of higher-order functions supports the manipulation and management of objects independent of their representation.

The combination of projection, shadowing and higher-order behaviour collectively provides a functionally implemented basis for modeling in an object-oriented style without embracing the rigid inheritance hierarchies, compiled object definitions or non-generic behaviour patterns.


Copyright (c) 2014, DuoMark International, Inc.
All rights reserved.
