Java properties mode for Emacs
===

Allows under _a very specific sets of circumstances_ to safely delete unreferenced java resources along with the accompanying files.

It's a little project I'm working in my spare time as a hobby, to learn emacs lisp, and to make my experience with java resources more pleasant and safe, and has probably little use for anyone else.

Installation
===

Copy the files in "code" to a `load-path` directory and add

    (require 'jproeprty)
    (add-hook 'find-file-hook 'jproperty-find-file-hook) 

in your config file to automatically turn the mode on.

It will highlight resources and provide a 

    jproperty-smart-delete-resource

command (bound to `C-c C-k` by default) that will remove resources and files (if associated) if not referenced by the java files in the same tree.

In order for this code to work, the structure of the files should have sibling `java` and `resources`.

License
===

The code in this repository is a demo/learning prop not intended for any use and not fit for any purpose.

This code is provided under a proprietary license. You can browse the code but not use it or distribute it (mainly to protect you, because it's crap and I don't want it to blow in your face).

