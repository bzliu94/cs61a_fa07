## CS 61A (Fall 2007)

Taken with Prof. Brian Harvey.

Works with UCB Scheme.

Course is about LISt Processing and survey of many ideas in computer science regularized to use one syntactic-structure-less language, taking advantage of the language's bias towards transparency and modularity. Course is said to be about structure (nested and enlarged lists) and interpretation (reacting to nested and enlarged lists).

## How to use with Ubuntu 16.04 LTS

Note: STk Scheme must be coerced to expect a 32-bit system; 64-bit STk does not work.

### If you don't mind not building from source, modify a pre-compiled STk binary in a way that involves coercion.

  1. Get the RPM from UC Berkeley or from the folder "setup".

         wget http://inst.eecs.berkeley.edu/~scheme/precompiled/Linux/STk-4.0.1-ucb1.3.6.i386.rpm

     (If you are using a 32-bit system, install using this RPM; or continue if you have a 32-bit OR 64-bit machine to create a 32-bit .deb package.)

  2. Get alien, which will help with turning a .rpm into a .deb.

         sudo apt-get install alien

  3. Install 32-bit-related libraries.

         sudo apt-get install libsm6:i386
         sudo apt-get install libx11-6:i386
         sudo apt-get install libc6-i386 lib32stdc++6 lib32gcc1 lib32ncurses5 lib32z1

     (If libx11-6:i386 does not work, perhaps libx11-dev will.)

  4. Change the .rpm into a .deb, specifying that we wish to have the .deb addable for a 64-bit machine.

         fakeroot alien --target=amd64 STk-4.0.1-ucb1.3.6.i386.rpm

  5. Install the package.

         sudo dpkg -i --force-architecture stk_4.0.1-1_amd64.deb

### If you want to build STk as a 32-bit program using either a 32-bit machine (or especially a 64-bit machine), compile a binary in a way that involves coercion.

  1. Get the source from UC Berkeley or from the folder "setup".

         wget http://inst.eecs.berkeley.edu/~scheme/source/stk-1.3.6.tgz

  2. Unpack the source.

         tar zxvf stk-1.3.6.tgz -C stk-1.3.6

  3. Change into the folder "stk-1.3.6".

  4. Fix a typo in stk/Tcl/configure at line 3222; instead of:

         system=MP-RAS-`awk '{print $3}' /etc/.relid'`

     We have:

         system=MP-RAS-`awk '{print $3}' /etc/.relid`

  5. Run "cd stk", then "make clean".

  6. Make sure you are using gcc version less than 5 - say, 4.8.

         sudo apt-get install g++-4.8

     (Make sure this version is active via update-alternatives.)

  7. Make sure you are ready to build 32-bit on a 64-bit machine using compile and link flag "-m32".

         sudo apt-get install g++-4.8-multilib

  8. Install 32-bit-related libraries.

         sudo apt-get install libsm6:i386
         sudo apt-get install libx11-6:i386
         sudo apt-get install libc6-i386 lib32stdc++6 lib32gcc1 lib32ncurses5 lib32z1

     (If libx11-6:i386 does not work, perhaps libx11-dev will.)

  9. Change two settings in stk/Lib/STk.init; change at line 34:

         /usr/local/lib/slib/

     To:

         /usr/local/lib/stk/slib/

     And change at line 29:

         (define (implementation-vicinity) "/3.99.3/")

     To:

         (define (implementation-vicinity) "/usr/local/lib/stk/1.3.6/")

  10. In stk/configure, change references to "gcc"; change at line 1388:

          ac_cv_prog_ac_ct_CC="gcc"

      To:

          ac_cv_prog_ac_ct_CC="gcc -m32"

      And change at line 6265:

          if test "$CC" = "gcc" -a "$SH_CCFLAGS" != "" ; then

      To:

          if test "$CC" = "gcc -m32" -a "$SH_CCFLAGS" != "" ; then

  11. Run "/bash/sh configure --prefix=/usr/local".

  12. In stk/Makefile, change at line 57 "gcc" to "gcc -m32".

  13. Use "make" and "sudo make install".

### Follow the following steps for UCB Scheme extensions.

  1. Get the source from UC Berkeley or from the folder "setup" and unpack the source, if you haven't already.

         wget http://inst.eecs.berkeley.edu/~scheme/source/stk-1.3.6.tgz
         tar zxvf stk-1.3.6.tgz -C stk-1.3.6

  2. Change into the folder "stk-1.3.6".

  3. Make sure you have gcc version less than 5 - say, 4.8.

         sudo apt-get install gcc-4.8

     (Make sure this version is active via update-alternatives.)

  4. Run "cd ucb".

  5. Run "/bash/sh configure --prefix=/usr/local".

  6. In ucb/stkdb/Makefile, change at line 12:

         MKDIR		= @MKDIR@

     To:

         MKDIR		= /bin/mkdir

  7. To deal with documentation tex files, we need to downgrade texinfo to version 4.13. We will build it from source, which requires libncurses5-dev. Get the source and extract it in a suitable directory:

         sudo apt-get install libncurses5-dev
         wget http://ftp.gnu.org/gnu/texinfo/texinfo-4.13.tar.gz
         gzip -dc < texinfo-4.13.tar.gz | tar -xf -
         cd texinfo-4.13
         /bash/sh configure

  8. Modify texinfo-4.13 Makefile to change at line 307:

         LDFLAGS = @LDFLAGS@

     To:

         LDFLAGS = "-L${PREFIX}/lib -lncurses"

  9. Run the following in texinfo-4.13/ to finish installing texinfo 4.13:

         make
         sudo make install

  10. Run the following in ucb to install UCB extensions:

          make
          sudo make install

  11. Run "sudo mkdir /usr/local/lib/stk/slib/stkdb".

  12. From folder ucb/stkdb, run "sudo cp *.scm stkdb.el /usr/local/lib/stk/slib/stkdb".

  13. Run the following once (using sudo) to build slib catalog once:

          sudo stk-simply

### Prepare proper Emacs integration.

  1. Add to .emacs in home directory:

         (add-to-list 'load-path "/usr/local/lib/stk/slib/stkdb")
         (autoload 'scheme-mode "stkdb" "Debugger for STk." t)
         (autoload 'scheme-mode "cmuscheme" "Major mode for Scheme." t)
         (autoload 'run-scheme "cmuscheme" "Switch to interactive Scheme buffer." t)
         (setq auto-mode-alist (cons '("\\.scm" . scheme-mode) auto-mode-alist))

  2. In Emacs, set scheme-program-name to stk by adding line to .emacs:

         (custom-set-variables '(scheme-program-name "stk-simply"))

  3. Add the following line to .emacs:

         (load "stkdb")

  3. One may wish to make all files editable

  4. For /usr/local/lib/stk/slib/stkdb/stkdb.el, replace at line 275:

         (mapc '(lambda (x)

     With:

         (mapc #'(lambda (x)

     (One may wish to make the contents of /usr/local/lib/stk/slib/stkdb editable via root or via a non-root user by first using chmod.)

### For Ubuntu 18.04 LTS

  1. Some other package such as libx11-dev:i386 for building STk may be necessary.

  2. In later versions of Ubuntu, Emacs 23 is no longer obtainable via package. For versions 24 and up (and in particular version 26), there may be a bug with empty Debugging and Scheme dynamically added menus when editing scheme files. To get around this issue, we add the following lines to .emacs file:

         ; deal with missing contents of menus
         (defun prepare-menus-for-scheme ()
         (menu-bar-mode -1)
         (menu-bar-mode 1))
         (add-hook 'scheme-mode-hook 'prepare-menus-for-scheme)

## Screenshot

<img src="https://raw.githubusercontent.com/bzliu94/cs61a_fa07/master/setup/emacs_ucb_scheme.png" alt="emacs with ucb scheme" width="400">

## Caveat emptor

This approach has been successful for a machine purchased in ~2014, but when we tried to use it for a machine purchased in 2007, we initially encountered segfaults when running certain programs with stk-simply. Then, after attempts to debug the problem, almost miraculously the problem disappeared. We are unable to reproduce the problem and as a result we are unable to diagnose it. As a warning, this approach may not work for you.

The problem involves report-error!!, message "segmentation fault: returning to toplevel", parallel-execute (whose implementation is provided by UCB Scheme extensions and whose concept comes from SICP section 3.4). We suspect it has to do with compiling STk and UCB Scheme extensions on 32-bit architecture and stale Makefile files, but again we do not know for sure.


