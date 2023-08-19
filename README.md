Destiny Inventory Tool
======================

An item manager for Destiny 2.
Destiny 1 support is planned, but has not been implemented.

Initial Setup
-------------
Building may be performed via Nix or via Alire.
Both are supported, however Alire is used for first-time setup of variables.

Using Alire
-----------
Install GTK and OpenSSL systemwide, then install Alire.

Uncomment Alire dependency lines in alire.toml

`alr build` or for development `alr exec bash` and then `gprbuild -j0`

Custom versions of AWS and Gtkada are bundled until an upstream source conflict is addressed in the next release (Gtkada)
and (for AWS) the build system is fixed to work with AWS (currently config is not saved when building via GPRBuild)

Tickets have been submitted upstream for both issues.

Using Nix
---------
You may optionally use Nix and Alire simultaneously: `nix-shell nix/alire.nix`, in which case see above.

Otherwise, enter the build environment: `nix-shell nix`

Usage
-----
Create a self-signed certificate named "cert.pem" in the dat/ subdirectory containing both a public and private key.
For example: `openssl req -newkey rsa:2048 -nodes -keyout key.pem -x509 -days 365 -out certificate.pem` and then `cat key.pem certificate.pem > cert.pem`

Open the application and follow instructions to use :)

You may need to allow the tool to use TCP port 8888 in order for the authentication process to complete, but this
is only required for initial setup. You can optionally perform authentication on a different computer and copy the token file.

It is also likely the browser will present a security error when you try to log in: please accept this and continue, as the insecure page is hosted on your local computer and does not present a risk.

Please create an Issue for any errors you experience, along with the full output of the programme.  

Contributing
------------
Ada is a somewhat niche language, and GUI programming / game-related work is seldom done in it.
If you have a feature to suggest though or some prototype code, no matter the language, please open
an Issue or Pull Request so that it can be incorporated.

The project can also take code in C++, C, Rust, etc., provided that you can make C or C++ bindings for it.

Forks
-----
As per the licence, this project may be forked, however please take care to change the API keys in "src/secrets/constant_secrets.ads".  
These are bound by the Bungie API Terms of Service, which are much narrower than the GPL / LGPL.

Licence
-------
Destiny Inventory Tool is licensed under the GNU GPL Version 3.  
The API code ("gpr/bungie_api.gpr") may optionally be used under the GNU LGPL Version 3.
