Ariadne
=======

Ariadne provides a "go-to-definition" functionality for Haskell.

[Video demonstration](http://youtu.be/-sbGijbhxAc)

Usage
-----

To use Ariadne, you need two things:

* install this package, `ariadne`, which includes the `ariadne-server`
  executable, and make sure this executable is running;
* find and install a plugin for your editor or IDE of choice.

Editor plugins
--------------

Currently, the following editor/IDE plugins exist:

* [vim](https://github.com/feuerbach/ariadne-vim)
* [emacs](https://github.com/manzyuk/ariadne-el)

Limitations
-----------

As of v0.1, Ariadne only knows about the current file. It won't find definitions
in other files in the same directory, or in the installed packages. It also
won't look in the Cabal file for the language extensions, and a file may fail to
parse because of that.

These will be addressed in the future versions.

Creating a plugin
-----------------

Writing a new Ariadne plugin should be straightforward (assuming you
know how to extend your editor/IDE).

If you write a new plugin, let me know so I can update the list above, and also
notify you when the protocol changes.

[bert]: http://bert-rpc.org/

### Conventions

In the protocol description below, we don't use Erlang's upper-case/lower-case
convention, because it would confuse anyone except Erlang or Prolog programmers.
Instead, variables and functions are written lowercase, and atoms are prefixed
with the colon, e.g. `:atom`.

### Protocol

You communicate with the Ariadne server via the [BERT-RPC protocol][bert] over
TCP. The server listens on the local TCP port 39014. The BERT-RPC module is
`ariadne`.

The request has form

    find(file, line, column)

where `file` is a binary string, `line` and `column` are integers. `file` must
contain the full and canonical path to the Haskell source file. It is assumed to
be UTF-8 encoded, although this may improve in the future.

The `line` and `column` should probably be the current cursor position. Ariadne
will look up the name at that location. Lines and columns are numbered starting
from 1.

The possible responses are:

    { :no_name }

This means that there's no recognized name at the
given position. The plugin should probably do nothing in this case.

    { :loc_known, file, line, column }

This means that the name is defined at the given file, line, and column. The
`file` is again a binary UTF-8 encoded full path. The plugin should probably
jump at that location.

    { :loc_unknown, modname }

We don't know where the name is defined, but we know it comes from the given
module (binary UTF-8 encoded name). The plugin may want to show this
information to the user.

    { :error, message }

Some error has occurred. For example, the file has a syntax or scoping error.
The plugin is expected to present the error message to the user.

The message is a binary UTF-8 encoded text, possibly spanning multiple lines.

Other requests and responses will probably be added in the future versions.
