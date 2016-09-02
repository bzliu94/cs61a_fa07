## CS 61A (Fall 2007)

Taken with Prof. Brian Harvey.

Works with UCB Scheme.

## How to use with Ubuntu 64-bit

    wget http://inst.eecs.berkeley.edu/~scheme/precompiled/Linux/STk-4.0.1-ucb1.3.6.i386.rpm

    sudo apt-get install alien

    sudo apt-get install libsm6:i386
    sudo apt-get install libx11-6:i386
    sudo apt-get install libc6-i386 lib32stdc++6 lib32gcc1 lib32ncurses5 lib32z1

    fakeroot alien --target=amd64 STk-4.0.1-ucb1.3.6.i386.rpm

    sudo dpkg -i --force-architecture stk_4.0.1-1_amd64.deb


