## CS 61A (Fall 2007)

Taken with Prof. Brian Harvey.

Works with UCB Scheme.

## How to use with Ubuntu 64-bit 16.04 LTS

For STk binary:

    wget http://inst.eecs.berkeley.edu/~scheme/precompiled/Linux/STk-4.0.1-ucb1.3.6.i386.rpm

    sudo apt-get install alien

    sudo apt-get install libsm6:i386
    sudo apt-get install libx11-6:i386
    sudo apt-get install libc6-i386 lib32stdc++6 lib32gcc1 lib32ncurses5 lib32z1

    fakeroot alien --target=amd64 STk-4.0.1-ucb1.3.6.i386.rpm

    sudo dpkg -i --force-architecture stk_4.0.1-1_amd64.deb

Note that if we are after STK built from source (which I did not succeed at because we get "core dumped" at run time), there is at least this change:

    Typo in stk/Tcl/configure file at line 3222; instead of:

    system=MP-RAS-`awk '{print $3}' /etc/.relid'`

    We have:

    system=MP-RAS-`awk '{print $3}' /etc/.relid`

For UCB Scheme extensions:

    wget http://inst.eecs.berkeley.edu/~scheme/source/stk-1.3.6.tgz

    tar zxvf stk-1.3.6.tgz -C stk-1.3.6

    cd stk-1.3.6

    (Make sure you have gcc version less than 5 - say, 4.8)

    cd ucb

    ./configure --prefix=/usr/local
    make
    sudo make install

    (Set @MKDIR@ to /bin/mkdir in ucb/stkdb/Makefile)

    (Downgrade texinfo to version 4.13 by building from source, which requires libncurses5-dev)

    (Modify texinfo-4.13 Makefile to have: LDFLAGS = "-L${PREFIX}/lib -lncurses")

    sudo mkdir /usr/local/lib/stk/slib/stkdb

    sudo cp stkdb/*.scm stkdb/stkdb.el /usr/local/lib/stk/slib/stkdb

For proper Emacs integration:

    Add to .emacs in home directory:

    (autoload 'scheme-mode "cmuscheme" "Major mode for Scheme." t)

    (add-to-list 'load-path "/usr/local/lib/stk/slib/stkdb")
    (autoload 'scheme-mode "stkdb" "Debugger for STk." t)

    (autoload 'run-scheme "cmuscheme" "Switch to interactive Scheme buffer." t)
    (setq auto-mode-alist (cons '("\\.scm" . scheme-mode) auto-mode-alist))

    In Emacs, set scheme-program-name to stk by adding line to .emacs:

    (custom-set-variables '(scheme-program-name "stk"))

    For stkdb.el, replace '(lambda with #'(lambda.


