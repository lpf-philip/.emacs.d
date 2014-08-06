 Hide/show comments in code.


 Macros defined here:

   `with-comments-hidden'.

 Commands defined here:

   `hide/show-comments', `hide/show-comments-toggle'.

 User options defined here:

   `hide-whitespace-before-comment-flag', `ignore-comments-flag'.


 Put this in your init file (`~/.emacs'):

  (require 'hide-comnt)


 Note for Emacs 20: The commands and option defined here DO NOTHING
 IN EMACS 20.  Nevertheless, the library can be byte-compiled in
 Emacs 20 and `hide-comnt.elc' can be loaded in later Emacs
 versions and used there.  This is the only real use of this
 library for Emacs 20: it provides macro `with-comments-hidden'.
