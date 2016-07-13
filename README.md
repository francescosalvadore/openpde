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
+ [VTKFortran](https://github.com/szaghi/VTKFortran)'s [licenses](https://github.com/szaghi/VTKFortran#copyrights)

Go to [Top](#top)

## Download

Openpde home is at [https://github.com/francescosalvadore/openpde](https://github.com/francescosalvadore/openpde). It uses `git submodule` to handle third party dependencies. To download all the source files you can:

##### clone recursively the repository

```bash
git clone --recursive https://github.com/francescosalvadore/openpde
git submodule update --init --recursive
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

Note that Besides this README file openpde's documentation is contained into its own [wiki](https://github.com/francescosalvadore/openpde/wiki). Detailed documentation of the API is contained into the [GitHub Pages](http://francescosalvadore.github.io/openpde/index.html) that can also be created locally by means of [ford tool](https://github.com/cmacmackin/ford).

Openpde's design is strongly based on abstract and concrete classes: each key-feature of PDEs is represented by a *class family*, namely an abstract class and (some of) its concrete extensions, that provides all the *behaviorual methods* to describe the PDE and to integrate it in space and time. In particular, PDEs are generally constituted by a (complex) *mixture* of the following *ingredients*:

+ one or more integrand *fields*, e.g. density, momentum and energy are the integrand field of the Euler's PDE;
+ one or more *spatial operators* operating on the fields that concur to define the *residual function* (or the whole spatial operator) of the PDE, e.g. the *divergence* of the conservative fluxes of density, momentum and energy is the spatial operator of the Euler's PDE; these must have both an exact definition and a numerical approximation;
+ a discrete representation of the integrand-spatial-domain, that is general indicated as *mesh*;
+ an integrator, that by means of a numerical approximation of the residual function can compute a (numerical approximation) new field solution at the new time.

Openpde takes care to define an abstract class for each key-ingredients and it also provides all the concrete counterparts to solve numerically the PDE: the users must only define their own PDE equation by means of the defined *operators*.

### Project Tree structure

The project tree is the following:

```bash
.
├── doc
│   └── ...
├── src
│   ├── lib
│   ├── tests
│   └── third_party
├── third_party
│   └── ...
├── tools
│   └── ...
...
```
+ `doc` contains the files necessary to build the API documentation;
+ `src` contains source files necessary to build the library and the tests;
+ `third_party` contains third party repositories as git submodules;
+ `tools` contains auxiliary tools, e.g. post-processing scripts.

#### `src` tree, the sources structure

The `src` tree structure is the following:

```bash
src/
├── lib
│   ├── openpde.f90
│   ├── openpde_kinds.f90
│   ├── openpde_equation
│   │   └── ...
│   ├── openpde_field
│   │   └── ...
│   ├── openpde_integrator
│   │   └── ...
│   ├── openpde_mesh
│   │   └── ...
│   └── openpde_spatial_operator
│       └── ...
├── tests
│   └── ...
└── third_party
    ├── json-fortran
    │   ├── json_file_module.F90 -> ../../../third_party/json-fortran/src/json_file_module.F90
    │   └── ...
    ├── VTKFortran
    │   ├── befor64.F90 -> ../../../third_party/VTKFortran/src/lib/befor64.F90
    │   └── ...
    └── ...
```

+ `src/lib` contains the openpde library sources;
+ `src/tests` contains the openpde tests sources;
+ `src/third_party` contains the link to third party sources.

#### `src/lib` tree, the openpde library sources structure

Openpde is based on some *abstract classes*:

+ [mesh](#mesh), abstract and concrete classes;
+ [field](#field), abstract and concrete classes;
+ [spatial operator](#spatial-operator), abstract and concrete classes;
+ [equation](#equation), only abstract class;
+ [integrator](#integrator), abstract and concrete classes;

Essentially, the openpde's design should allow two different workflows for the library developers and for the library users:

+ *developers*: could modify/create all classes;
+ *users*: should modify/create only concrete extensions of [equation](#equation) abstract class.

In the following a short description of the classes meaning is provided.

##### mesh

The abstract mesh represents the most generic `mesh` in the sense of PDE numerical integration (in space and time). Essentially, this object is empty: how to store mesh data and how to use it are left to concrete extensions of the abstract `mesh`. Currently, openpde provides the following concrete meshes:

###### finite difference, 1D/2D, mesh

The concrete class `mesh_FD_1D/2D` is designed to represent a finite difference, 1D/2D mesh: it is uniform, one/two dimensional discretization of the integration domain. To handle boundary conditions, ghost cells technique is exploited. The data provided by this object are used by the [field](#field) and [spatial operators](sptial-operator).

##### field

The abstract field represents the most generic `field` in the sense of PDE integrand (in space and time). Essentially, this object ships a pointer to the associated underling `mesh` and a far complete set of *operators* to combine fields and fields/reals. As a matter of facts, PDEs commonly involve some algebra on integrand fields.

The field *value* (into the whole mesh) is left to the *concrete* extensions of the abstract `field`. Currently, openpde provides the following concrete fields:

###### finite difference, 1D/2D, field

The concrete class `field_FD_1D/2D` is designed to represent a field of a finite difference, 1D/2D mesh. Consequently, its value is stored for each mesh points. The applicable spatial operators are those of the finite difference 1D/2D classes (i.e. `spatial_operator_xxx_FD_1D/2D` classes).

##### spatial operator

##### equation

The abstract equation contains the necessary data to express the PDE equation. Essentially, it is an object that provide the method `forcing` that compute the *residual function* of the PDE. This class is necessary by the time *integrators* for advance in time the PDE solution. The `forcing` method operates on a `field` class and returns a (forced by the residual function) field.

##### integrator

To be completed.

Go to [Top](#top)
