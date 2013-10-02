benchmark-el
============

This is a simple benchmark of calls to Emacs require and load functions.
It can be used to keep track of where time is being spent during Emacs
startup in order to optimize startup times.

The code was originally based on [init-benchmarking.el][1] by Steve Purcell but
several modification has gone into it since.


Installation
------------

Place this program in your load path and add the following code to the
beginning of your Emacs initialization script.

```lisp
(require 'benchmark)
(benchmark/install)
```


Usage
-----

After Emacs has finished loading the following two functions can be called
in order to display the results.

 - benchmark/show-require-times
 - benchmark/show-load-times

[ctable][2] is used to display the results and must be in your Emacs load path
for the functions to work.


[1]: https://github.com/purcell/emacs.d/blob/master/init-benchmarking.el
[2]: https://github.com/kiwanami/emacs-ctable
