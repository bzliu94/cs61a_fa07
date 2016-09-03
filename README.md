## CS 61A (Fall 2007)

Taken with Prof. Brian Harvey.

Works with UCB Scheme.

## How to use with Ubuntu 64-bit 16.04 LTS

1. If you don't mind not building from source, prepare the STk binary.

  1. Get the RPM from UC Berkeley or from the folder "setup".

            wget http://inst.eecs.berkeley.edu/~scheme/precompiled/Linux/STk-4.0.1-ucb1.3.6.i386.rpm

  2. Get alien, which will help with turning a .rpm into a .deb.

            sudo apt-get install alien

  3. Install 32-bit-related libraries.

            sudo apt-get install libsm6:i386
            sudo apt-get install libx11-6:i386
            sudo apt-get install libc6-i386 lib32stdc++6 lib32gcc1 lib32ncurses5 lib32z1

  4. Change the .rpm into a .deb, specifying that we wish to have the .deb addable for a 64-bit machine.

            fakeroot alien --target=amd64 STk-4.0.1-ucb1.3.6.i386.rpm

  5. Install the package.

            sudo dpkg -i --force-architecture stk_4.0.1-1_amd64.deb

2. If you want to build STk as a 32-bit program using a 64-bit machine, create the binary.

  1. Get the source from UC Berkeley or from the folder "setup".

            wget http://inst.eecs.berkeley.edu/~scheme/source/stk-1.3.6.tgz

  2. Unpack the source.

            tar zxvf stk-1.3.6.tgz -C stk-1.3.6

  3. Change into the folder "stk-1.3.6".

  3. Fix a typo in stk/Tcl/configure at line 3222; instead of:

            system=MP-RAS-`awk '{print $3}' /etc/.relid'`

      We have:

            system=MP-RAS-`awk '{print $3}' /etc/.relid`

  4. Run "cd stk", then "make clean".

  5. Make sure you are using gcc version less than 5 - say, 4.8.

            sudo apt-get install g++-4.8   

  6. Make sure you are ready to build 32-bit on a 64-bit machine using compile and link flag "-m32".

            sudo apt-get install g++-4.8-multilib

  7. Install 32-bit-related libraries.

            sudo apt-get install libsm6:i386
            sudo apt-get install libx11-6:i386
            sudo apt-get install libc6-i386 lib32stdc++6 lib32gcc1 lib32ncurses5 lib32z1

  8. Change two settings in stk/Lib/STk.init; change at line 34:

            /usr/local/lib/slib/

      To:

            /usr/local/lib/stk/slib/

      And change at line 29:

            (define (implementation-vicinity) "/3.99.3/")

      To:

            (define (implementation-vicinity) "/usr/local/lib/stk/1.3.6/")

  9. In stk/configure, change references to "gcc"; change at line 1388:

            ac_cv_prog_ac_ct_CC="gcc"

      To:

            ac_cv_prog_ac_ct_CC="gcc -m32"

      And change at line 6265:

            if test "$CC" = "gcc" -a "$SH_CCFLAGS" != "" ; then

      To:

            if test "$CC" = "gcc -m32" -a "$SH_CCFLAGS" != "" ; then

  10. Run "./configure --prefix=/usr/local".

  11. In stk/Makefile, change at line 57 "gcc" to "gcc -m32".

  12. Use "make" and "sudo make install".

3. Follow the following steps for UCB Scheme extensions.

  1. Get the source from UC Berkeley or from the folder "setup" and unpack the source, if you haven't already.

            wget http://inst.eecs.berkeley.edu/~scheme/source/stk-1.3.6.tgz
            tar zxvf stk-1.3.6.tgz -C stk-1.3.6

  2. Change into the folder "stk-1.3.6".

  3. Make sure you have gcc version less than 5 - say, 4.8.

            sudo apt-get install gcc-4.8

  4. Run "cd ucb".

  5. Run "./configure --prefix=/usr/local".

  6. In ucb/stkdb/Makefile, change at line 12:

            MKDIR		= @MKDIR@

      To:

            MKDIR		= /bin/mkdir

  7. To deal with documentation tex files, we need to downgrade texinfo to version 4.13. We will build it from source, which requires libncurses5-dev. Get the source and extract it in a suitable directory:

            sudo apt-get install libncurses5-dev
            wget http://ftp.gnu.org/gnu/texinfo/texinfo-4.13.tar.gz
            gzip -dc < texinfo-4.13.tar.gz | tar -xf -
            cd texinfo-4.13
            ./configure

  8. Modify texinfo-4.13 Makefile to change at line 307:

            LDFLAGS = @LDFLAGS@

      To:

            LDFLAGS = "-L${PREFIX}/lib -lncurses")

  9. Run the following in texinfo-4.13/ to finish installing texinfo 4.13:

            make
            sudo make install

  10. Run the following in ucb/stkdb/ to install stkdb:

            make
            sudo make install

  11. Run "sudo mkdir /usr/local/lib/stk/slib/stkdb".

  12. Run "sudo cp stkdb/*.scm stkdb/stkdb.el /usr/local/lib/stk/slib/stkdb".

4. Prepare proper Emacs integration.

  1. Add to .emacs in home directory:

            (add-to-list 'load-path "/usr/local/lib/stk/slib/stkdb")
            (autoload 'scheme-mode "stkdb" "Debugger for STk." t)
            (autoload 'scheme-mode "cmuscheme" "Major mode for Scheme." t)
            (autoload 'run-scheme "cmuscheme" "Switch to interactive Scheme buffer." t)
            (setq auto-mode-alist (cons '("\\.scm" . scheme-mode) auto-mode-alist))

  2. In Emacs, set scheme-program-name to stk by adding line to .emacs:

            (custom-set-variables '(scheme-program-name "stk-simply"))

  3. For /usr/local/lib/stk/slib/stkdb/stkdb.el, replace at line 275:

            (mapc '(lambda (x)

      With:

            (mapc #'(lambda (x)


