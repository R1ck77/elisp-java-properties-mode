Java properties mode for Emacs
===

Allows under _a very specific sets of circumstances_ to safely delete unreferenced java property entries along with the accompanying files.

It's a little project I'm working in my spare time as a hobby, to learn emacs lisp, and to make my experience with java properties more pleasant and safe, and has probably little use for anyone else.

Installation
===

Copy the files in "code" to a `load-path` directory and add

    (require 'jproperty)
    (add-hook 'find-file-hook 'jproperty-find-file-hook) 

in your config file to automatically turn the mode on when loading files ending with `.resources` and `.properties`.

It will highlight properties and provide a couple of commands.

Commands 
====

    jproperty-smart-delete-resource

will remove properties and files associated with them if not referenced by the java files in the same tree. Bound to `C-c C-k` by default.

In order for this code to work, the structure of the files should have sibling `java` and `resources`.

    jproperty-check-key-of-current-property
    
will check the key of the property at point, and change the color of the key if not referenced by any java file in the subree (same considerations of the previous command about the filesystem layout applies). Bound to `C-c C-v` by default.

    jproperty-check-all-keys-in-file
    
Like `jproperty-check-key-of-current-property`, but for each line in the current buffer. Bound to `C-c C-a` by default.

License
===

The code in this repository is a demo/learning prop not intended for any use and not fit for any purpose.

This code is provided under a proprietary license. You can browse the code but not use it or distribute it (mainly to protect you, because it's crap and I don't want it to blow in your face).

