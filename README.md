Fre(e)qShow
===========

Introduction
------------
Fre(e)qShow is a utility to display CPU frequency information for each logical
CPU on a Linux system.

The initial purpose for the project was to be a programming excersise in 
Haskell and various HTML5 technologies.

Usage
-----
Build by typing 'make'.

Run by typing 'sudo ./freeqshow'. This will run the server with the 
default ports for http (= 8000) and web sockets (= 9160).

To change port number(s) use the switches '-http <port>' or '-ws <port>.

Point a HTML5 compatible webb browser (tested with newer Chrome, Firefox 
and Safari browsers) at your server. It will graphically show your server's
frequency information.

NB: The server must run as sudo due to file permissions.

Dependencies
------------
The Haskell part is using a couple of nice Haskell libraries, all available 
through cabal:

blaze-html (HTML templating)
happstack-lite (Web server)
websockets (Web socket server)
aeson (JSON coder/decoder)

