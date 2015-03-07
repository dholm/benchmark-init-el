# benchmark-init

This is a simple benchmark of calls to Emacs require and load functions.
It can be used to keep track of where time is being spent during Emacs
startup in order to optimize startup times.

The code was originally based on [init-benchmarking.el][1] by Steve Purcell but
several modification has gone into it since.


## Installation

Run `make` inside the directory where you installed *benchmark-init*, this will
produce the `benchmark-init-loaddefs.el` file.  Then place the following code
as early as possible in your Emacs initialization script.  Replace
`/path/to/benchmark-init` with the path to the directory where you put
*benchmark-init*.

```lisp
(add-to-list 'load-path "/path/to/benchmark-init/")
(require 'benchmark-init-loaddefs)
(benchmark-init/activate)
```

Data collection will begin immediately after the call to
`benchmark-init/activate`.


### Using el-get

If you are not using *el-get* to manage your Emacs packages you can skip this
section.

Since benchmark-init must be activated as early as possible so that it can
measure calls to load and require it should be loaded before *el-get* starts
bringing in other packages.  To achieve that, add something like the following
snippet as early as possible in your Emacs initialization script, before
calling *el-get*.  Replace `/path/to/el-get` with the path to your *el-get*
directory.

```lisp
(let ((benchmark-init.el "/path/to/el-get/benchmark-init/benchmark-init.el"))
  (when (file-exists-p benchmark-init.el)
    (load benchmark-init.el)))
```

The first time you start Emacs after adding this nothing will be benchmarked
since *el-get* will only install the package.  Simply quit and restart Emacs
and everything should be benchmarked from now on.


## Usage

There are two ways in which benchmark-init's results can be presented, as a
table or in a tree.  The table can be displayed by running:

 - benchmark-init/show-durations-tabulated

Which will bring up the results in a tabulated list:

```
| Module                       |  Type   | ms [^] | total ms |
+------------------------------+---------+--------+----------+
| eldoc-eval                   | require |    204 |      204 |
| eldoc                        | require |    183 |      183 |
| semantic/db-find             | require |     19 |       92 |
| ispell                       | require |     16 |       16 |
| grep                         | require |      6 |        6 |
| ~/.emacs.d/benchmark-init.el | load    |      1 |        1 |
```

The *ms* column lists the amount of time spent loading the entry itself and
*total ms* is the duration spent loading the entry and its dependencies.  In
the tree mode each entry will only display the time spent loading the entry
itself, not including children.

Tree mode can be displayed by running:

 - benchmark-init/show-durations-tree

```
╼►[benchmark-init/root nil 0ms]
  ├─[benchmark-init-modes require 8ms]
  ├─[eldoc-eval require 2ms]
  │ ╰─[eldoc require 125ms]
  ├─[~/.emacs.d/el-get/benchmark-init/benchmark-init.el load 4ms]
  ╰─[auto-dictionary require 72ms]
    ╰─[flyspell require 9ms]
      ╰─[ispell require 24ms]
```

It is possible to control when benchmark-init should collect data by using the
following two functions:

 - benchmark-init/activate
 - benchmark-init/deactivate

[1]: https://github.com/purcell/emacs.d/blob/master/lisp/init-benchmarking.el
