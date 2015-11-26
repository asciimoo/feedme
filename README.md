# feedme

Feedme is a lightweight, single-user feed reader implemented in Common Lisp


## Setup


### Install

install dependencies via quicklisp:

```common-lisp
(ql:quickload '(:drakma :xmls :woo :clack :ningle :cl-markup :crane))

```

add `feedme.asd` to your lisp system directory:

```shell
$ ln -s /path/to/your/feedme/feedme.asd /path/to/your/cl/systems/feedme.asd
```

### Load and start webapp


```common-lisp
(require "feedme")
(feedme:start-webapp)
```
Now you can access to http://localhost:5000/


TODO: create a quicklisp package from feedme

## Bugs

Bugs or suggestions? Visit the [issue tracker](https://github.com/asciimoo/feedme/issues).


## [LICENSE](https://github.com/asciimoo/feedme/blob/master/LICENSE)

## Notes

Only tested with SBCL
