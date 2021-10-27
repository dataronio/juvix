# Installation

## Prerequisites

* Install [Stack](https://haskellstack.org):

  - For Ubuntu        : `apt install stack`
  - For Debian        : `apt install haskell-stack`
  - For Arch Linux    : `pacman -S stack`
  - For macOS : `brew install haskell-stack`
  - For Windows, following the instructions
  [here](https://docs.haskellstack.org/en/stable/install_and_upgrade/#windows).
  
  It is required at least 8GB RAM for `stack` installation.

* You might need to install the following third-party libraries.

  - [libff](https://github.com/scipr-lab/libff),
  - [libsecp256k1](https://github.com/bitcoin-core/secp256k1), and
  - [openssl libssl](https://wiki.openssl.org/index.php/Libssl_API).


## Installing Juvix

1. To install Juvix, you can download its sources using
   [Git](http://git-scm.com/) from the [Github
   repository](https://github.com/anoma/juvix.git). Then, the program
   can be downloaded and installed with the following commands:

   ````bash
   $ git clone https://github.com/anoma/juvix.git
   $ cd juvix
   $ stack install
   ````
   
   If the installation succeeds, you must be able to run the juvix command
   from any location. To get the complete list of commands, please run `juvix --help`.
   
   ```
   $  juvix version
   Juvix version 0.1.1.19-a2111a3
   Branch: develop
   Commit: a2111a389be0e17291f2903d1ec7d7b94bd60cf8
   Date: Mon Sep 13 13:26:18 2021 -0500
   ```

2. For a more advanced user, after cloning Juvix into a local
   directory, to build and install the binary to the local path, you
   can run the following make commands: `make` or for full optimisations
   (but slower compile times): `make build-prod`.

   For Windows users, to use the command *make*, please visit [this
   link](https://stackoverflow.com/questions/32127524/how-to-install-and-use-make-in-windows).
   