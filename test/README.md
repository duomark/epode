Testing Approach
================

This project uses a combination of common_test and PropEr to validate the library. Common_test provides the mechanism for large-scale testing and configuring with multiple backends as well as providing clean, easy to read HTTP-based test results. PropEr is used for property-based testing which more thoroughly validates the abstractions and concepts behind the library. Traditional unit testing tends to lead to brittle tests that are more tightly bound to the implementation, and one of the key tenets here is to provide a generalized abstraction that can be used in many unrelated applications.

It is assumed that the dictionary implementations underlying epode are tested thoroughly in their respective original libraries. The testing in this library is designed to show that it has consistent interfaces, allows for clean embedding in an application and that any semantic transformations imposed by the epode approach are well implemented. The testing is focused on the behaviours and abstractions of an integrated application.

Test Layers
-----------

The following layers are tested independently:

  1. Backing store layer (dets, redis and mecked redis)
  2. Validation of DB epodes and defaults
  3. Conversion of DB epodes to App epodes

