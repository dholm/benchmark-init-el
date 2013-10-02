# benchmark-init

This is a simple benchmark of calls to Emacs require and load functions.
It can be used to keep track of where time is being spent during Emacs
startup in order to optimize startup times.

The code was originally based on [init-benchmarking.el][1] by Steve Purcell but
several modification has gone into it since.


## Installation

Place this program in your load path and add the following code to the
beginning of your Emacs initialization script.

```lisp
(require 'benchmark-init)
(benchmark/install)
```


### Using el-get

Add the following recipe to el-get.

```lisp
(:name benchmark-init
       :type github
       :depends (ctable)
       :pkgname "dholm/benchmark-init-el")
```

Since benchmark-init must be installed as early as possible so that it can
measure calls to load and require it should be loaded before *el-get* starts
bringing in other packages. To achieve that add something like the following
snippet as early as possible in your Emacs initialization script, before
calling *el-get*.

```lisp
(let ((benchmark-init-path "/path/to/el-get/benchmark-init"))
  (when (file-exists-p benchmark-init-path)
    (add-to-list 'load-path benchmark-init-path)
    (require 'benchmark-init)
    (benchmark/install)))
```

The first time you start Emacs after adding this nothing will be benchmarked
since *el-get* will only install the package. Simply quit and restart Emacs and
everything should be benchmarked from now on.


## Usage

After Emacs has finished loading the following two functions can be called
in order to display the results.

 - benchmark/show-require-times
 - benchmark/show-load-times

[ctable][2] is used to display the results and must be in your Emacs load path
for the functions to work.


[1]: https://github.com/purcell/emacs.d/blob/master/init-benchmarking.el
[2]: https://github.com/kiwanami/emacs-ctable
