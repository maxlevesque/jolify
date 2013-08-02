jolify
======

Jolify is a Fortran code prettifier.
Jolify does:
- remove multi-spaces or tabs
- insert spaces before and after :: .eqv. == >= etc.
- indent (default is four spaces by indentation level
- verify max length of 132 char per line (fortran default)


Author
======

Jolify is written by Maximilien Levesque (maximilien.levesque@gmail.com).


HowTo
=====

Jolify has been tested only with linux.
Jolify is written in modern fortran.
You can build it manually with gfortran,
or you can use `scons` to automate this task. In fact, I wrote the *scons script* for you.
First, install `scons` on your computer. Scons is a modern equivalent to GNU Make.
Installation of scons requires administrator rights, and may be done with `sudo yum install scons` in Fedora,
or `sudo apt-get install scons` in Ubuntu. 

Then, just type `scons` from the Jolify dir, and Jolify will be build automaticaly. 
