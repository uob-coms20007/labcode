# The LabCode Repo

This repository contains the starter code for the practical exercises of the unit *COMS20007: Programming Languages and Computation*.  To get started, we recommend obtaining a clean install of the Haskell toolchain.

## Install the Haskell Toolchain

The following instructions will guide you through obtaining our recommended Haskell tools:

  * GHC 9.0.1
  * Cabal 3.4.0.0

### Linux, OS X, FreeBSD and Windows on WSL2

Use the `ghcup` tool.  Everything can be done within your own user account (no raised privileges necessary).

  1. Set some environment variables in your terminal ready for the `ghcup` installation:
  ```
      export BOOTSTRAP_HASKELL_NONINTERACTIVE=1
      export BOOTSTRAP_HASKELL_GHC_VERSION=9.0.1
  ```
  
  2. Install `ghcup` by following the instructions [here]( https://www.haskell.org/ghcup/) within the same terminal session.

     * If your system has an old version of `curl`, such as in certain releases of Centos, then the shell command given in the instructions will fail with an error such as: 
       ``` 
         curl: option --proto: is unknown
       ```
       In such a case, you can instead download the script at https://get-ghcup.haskell.org using your browser, save it somewhere accessible and with a name like `ghcup-init` and then run it from your shell with: 
       ```
         sh /path/to/ghcup-init
       ```

### Windows (not WSL2)
  
We recommend development on Windows Subsystem for Linux v2 ([WSL2](https://docs.microsoft.com/en-us/windows/wsl/about)).  If you want to install WSL2 on your Windows 10 machine, follow the instructions [here](https://docs.microsoft.com/en-us/windows/wsl/install-win10) before continuing as above.
  
However, you can also develop natively in Windows using the [chocolatey](https://chocolatey.org) package manager.  You will need admin privileges.
    
  1. Install the `chocolately` package manager by following the instructions at https://chocolatey.org/install.

  2. At a command prompt with Admin privileges, run:
     ```
       choco install haskell-dev 
       refreshenv
     ```

  3. The package adds to the end of your `PATH` environment variable so, if you have previously installed versions of GHC or Cabal, then you should edit your environment variables to remove them.  The versions installed by `chocolatey`, which you should retain, are likely:
     ```
       C:\Users\<USERNAME>\AppData\Roaming\cabal\bin
       C:\tools\ghc-9.01\bin
       C:\tools\msys64
     ```