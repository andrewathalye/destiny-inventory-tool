Destiny Inventory Tool
======================

An item manager for Destiny 2.
Destiny 1 support is planned, but has not been implemented.

Installation
------------
[TODO] Alire + GPRBuild  
Alire currently has some issues building both AWS and GTKAda in the same project.  
Additionally, AWS is building with OpenSSL support at the moment...

Tickets submitted upstream.

Usage
-----
This is the simplest bit, just open the application, go to the URL, and it should
download your profile and render your inventory.

Contributing
------------
Ada is a somewhat niche language, and GUI programming / game-related work is seldom done in it.
If you have a feature to suggest though or some prototype code, no matter the language, please open
an Issue or Pull Request so that it can be incorporated.

The project can also take code in C++, C, Rust, etc., provided that you can make C or C++ bindings for it
and integrate the build system into Alire.

Forks
-----
As per the licence, this project may be forked, however please take care to change the API keys in "src/secrets/constant_secrets.ads".  
These are bound by the Bungie API Terms of Service, which are much narrower than the GPL / LGPL.

Licence
-------
Destiny Inventory Tool is licensed under the GNU GPL Version 3.
The API code ("gpr/bungie_api.gpr") may optionally be used under the GNU LGPL Version 3.
