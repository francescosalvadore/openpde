<a name="top"></a>

# openpde [![GitHub tag](https://img.shields.io/github/tag/francescosalvadore/openpde.svg)]()

[![License](https://img.shields.io/badge/license-GNU%20GeneraL%20Public%20License%20v3%20,%20GPLv3-blue.svg)]()
[![License](https://img.shields.io/badge/license-BSD2-red.svg)]()
[![License](https://img.shields.io/badge/license-BSD3-red.svg)]()
[![License](https://img.shields.io/badge/license-MIT-red.svg)]()

[![Status](https://img.shields.io/badge/status-unstable-orange.svg)]()

> Some initial experiments with Fortran OO and PDEs

---

[What is openpde?](#what-is-openpde?) | [Main features](#main-features) | [Copyrights](#copyrights) | [Download](#download) | [Compilation](#compilation) | [Documentation](#documentation)

---

## What is openpde?

To be written.

Go to [Top](#top)

## Main features

To be written.

Go to [Top](#top)

## Copyrights

openpde is an open source project, it is distributed under a multi-licensing system:

+ for FOSS projects:
  - [GPL v3](http://www.gnu.org/licenses/gpl-3.0.html);
+ for closed source/commercial projects:
  - [BSD 2-Clause](http://opensource.org/licenses/BSD-2-Clause);
  - [BSD 3-Clause](http://opensource.org/licenses/BSD-3-Clause);
  - [MIT](http://opensource.org/licenses/MIT).

Anyone is interest to use, to develop or to contribute to openpde is welcome, feel free to select the license that best matches your soul!

More details can be found on [wiki](https://github.com/francescosalvadore/openpde/wiki/Copyrights).

### Third Parties

Openpde relies on some third party codes each having its own licence. See them for details:

+ [json-fortarn](https://github.com/jacobwilliams/json-fortran)'s [license](https://raw.githubusercontent.com/jacobwilliams/json-fortran/master/LICENSE)

Go to [Top](#top)

## Download

Openpde home is at [https://github.com/francescosalvadore/openpde](https://github.com/francescosalvadore/openpde). It uses `git submodule` to handle third party dependencies. To download all the source files you can:

##### clone recursively the repository

```bash
git clone --recursive https://github.com/francescosalvadore/openpde
```

##### download the latest master-branch archive

[https://github.com/francescosalvadore/openpde/archive/master.zip](https://github.com/francescosalvadore/openpde/archive/master.zip)

##### download a release archive

[https://github.com/francescosalvadore/openpde/releases](https://github.com/francescosalvadore/openpde/releases)

Go to [Top](#top)

## Compilation

Openpde is still under development phase: the sources change often and strongly, it being still in *alpha* test. As as consequence, an *inflexible* legacy makefile is not yet provided. To build the unstable library you must use the more practical [FoBiS.py](https://github.com/szaghi/FoBiS) building tool.

#### Build tests

To build all tests provided type

```bash
FoBiS.py build -mode tests-gnu         # build test with GNU gfortran
# or
FoBiS.py build -mode tests-gnu-debug   # build test with GNU gfortran with debug options
# or
FoBiS.py build -mode tests-intel       # build test with Intel Fortran
# or
FoBiS.py build -mode tests-intel-debug # build test with Intel Fortran with debug options
```

To build only a specific test, e.g. `burgers` one, type

```bash
FoBiS.py build -mode tests-gnu -t burgers.f90
```
You can also specify all other modes listed above.

#### Build only the library

To build the static linked library type

```bash
FoBiS.py build -mode opendiff-static-gnu         # build library with GNU gfortran
# or
FoBiS.py build -mode opendiff-static-gnu-debug   # build library with GNU gfortran with debug options
# or
FoBiS.py build -mode opendiff-static-intel       # build library with Intel Fortran
# or
FoBiS.py build -mode opendiff-static-intel-debug # build library with Intel Fortran with debug options
```

Go to [Top](#top)

## Documentation

Besides this README file openpde's documentation is contained into its own [wiki](https://github.com/francescosalvadore/openpde/wiki). Detailed documentation of the API is contained into the [GitHub Pages](http://francescosalvadore.github.io/openpde/index.html) that can also be created locally by means of [ford tool](https://github.com/cmacmackin/ford).

To be completed.

Go to [Top](#top)
