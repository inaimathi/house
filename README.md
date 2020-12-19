# :house
##### Minimal, asynchronous, native Common Lisp web server

[![Build Status](https://travis-ci.org/inaimathi/house.svg?branch=major-overhaul)](https://travis-ci.org/inaimathi/house)

The goals of `:house` are to be

1. Asynchronous
2. Minimal, but complete for the purposes of web application development
3. Fully native

Performance is _not_ on this list. This is the server/client framework you run if you never want to see an `ffi-binding` error on any platform where you're able to run `sbcl`. If you need high-throughput, take a look at [`woo`](https://github.com/fukamachi/woo). If you need a thread-per-request model, look into [`hunchentoot`](http://edicl.github.io/hunchentoot/). If you need a file server, go set up [`nginx`](https://www.nginx.com/).

### News

House has undergone a major overhaul. This shouls simplify a bunch of things.

### Installation

- If you haven't already done so, install [`quicklisp`](http://www.quicklisp.org/beta/)
- Hop into your lisp and run `(ql:quickload :house)`

### Usage
##### Quick start

    (define-handler (hello-world :content-type "text/plain") ()
      "Hello world!")
	(define-handler (hello-you/<name>=>>string) ()
	  (format nil "Hello there, ~a!" name))
	(house:start 4040)

You should then be able to hop over to a browser and visit `http://localhost:4040/hello-world` to see the plaintext `"Hello world!"` response, or `http:localhost:4040/hello-you/James` to see the plaintext `"Hello there, James!"` (this may or may not make sense to do, depending on whether your name is James).

##### Threaded quick start

Because `:house` is single-threaded, using `house:start` directly as above will monopolize your REPL. You might not care about that in certain circumstances, or perhaps your deployment environment can't afford the extra thread. If you do and it can, you should start `:house` in a separate thread to retain your ability to evaluate things against the running system. You can do that the usual way:

    (defparameter *server* (bordeaux-threads:make-thread (lambda () (house:start 4040))))

##### Static Files

**House is not a file server**. Static file serving capability is provided for ease of testing, and maybe for *very* small-scale deployment. If you're going to be taking any significant traffic, get a reverse proxy going with something like [`nginx`](http://www.cyberciti.biz/tips/using-nginx-as-reverse-proxy.html).

You can define a static file handler with

    (define-file-handler [file-or-directory])

It'll handle individual files by serving them, and it'll handle directories by serving all contained files recursively.

##### Redirecting

You can re-direct to a different handler by using the `redirect!` function inside of a `define-handler` call

    (define-handler (name) () (redirect! "/static/name.html" :permanent? t))

Requests for `"/name"` will now instead serve a `301 Moved Permanently` response with a target of `"/static/name.html"` (if you leave out `:permanent? t`, it'll be a `307 Temporary Redirect` instead). House isn't optimized for redirecting, either from the performance or the notation perspective. If you're going to be re-directing any significant number of pages, consider having your reverse proxy handling that too.

##### Using the type annotations

You can specify desired argument types for your handlers. For example:

    (define-handler (handler) ((foo >>json) (bar >>integer) (baz (>>list #'>>keyword)))
       ...)

You can then use `bar` as an integer, `foo` as a parsed JSON s-expression and `baz` as a parsed `list` of `keyword`s  in the body of that handler. The built-in types are `>>string`, `>>integer`, `>>json`, `>>keyword`, and `>>list`. The last one is a higher-order function; it expects to output a sequence of the given type.

Type annotations are just named functions. You can define your own by defining a function of `String -> a` that parses a string into your desired type and possibly throws an `error` of some sort on failure. If you need a more specific type, you can use `defun`. For example:

	(defun >>game (param)
	  (let ((g (gethash param *game-table*)))
	    (assert (typep g 'game))
	    g))

Once that's done, you can annotate parameters with the `>>game` type.

    (define-handler (handler) ((foo >>game) ...) ...)

`foo` will then be looked up in `*game-table*`, and `assert`ed to be of type `'game` before the handler body is evaluated.

All this is entirely optional. If you don't care about it, just pass un-annotated arguments to your handlers, and they'll do exactly what you'd expect. You'll then be able to handle the type-conversion/assertions entirely manually.

### External API
#### Basics
###### `start`

Takes a port-number and starts the server listening on that port.

#### Handlers
###### `define-handler`

Defines a handler. The handler body has access to the bound symbol `request` in addition to its' parameters:

- `request`: the raw `request` object, with exported accessors `resource`, `headers` `session-tokens` and `paramteres`.
	- `paramters` contains the raw `alist` of incoming HTTP parameters
	- `resource` contains the raw URI, minus host and parameters
	- `headers` contains the raw HTTP headers `alist` (of particular interest is the `:host` key)
	- `session-tokens` contains the raw list of session tokens associated with this request
	- `session` contains the session
	- `socket-of` is the `socket` that generated the request. In principle, you could use this to set up some sort of communication layer that isn't strict `HTTP` if you like.

###### `define-json-handler`

Defines a `handler` that responds with `"application/json"`, and automatically JSON-encodes its response.

###### `define-channel`

Defines a `handler` that opens an [`EventSource`](https://developer.mozilla.org/en-US/docs/Web/API/EventSource) instead of closing after sending the response.

###### `define-file-handler`

Defines a handler that very inefficiently responds with static files.

#### Event Streams
###### `subscribe!`

Subscribes the specified socket to the specified channel. Should only be used with stream handlers, since the socket will need to be kept open for a subscription to be relevant.

###### `publish!`

Publishes a message to all subscribers of the specified channel. At the moment, it's assumed that the message recipients are all `EventSource` consumers (I'm working on a more general `WebSocket`-capable interface)
