# Commix

[![Build Status](https://travis-ci.org/vspinu/commix.svg?branch=master)](https://travis-ci.org/vspinu/commix)

> com•mix (kəˈmɪks)
>
>     to mix together, blend components

Commix is a Clojure (and ClojureScript) micro-framework for building
applications with data-driven architecture. It provides simple and idiomatic
composition of components and allows pipelines of custom life-cycle
actions.

Commix is an alternative to other life-cycle management
systems - [Component][], [Mount][] and [Integrant][].

[component]: https://github.com/stuartsierra/component
[mount]: https://github.com/tolitius/mount
[integrant]:https://github.com/weavejester/integrant
[dif-integrant]:https://github.com/vspinu/commix/wiki/Differences-with-Integrant

## Rationale

Commix was built as a response to a range of limitations in [Integrant][] which
in turn was designed to overcome limitations in
Component. See [Differences with Integrant][dif-integrant] for a walk through.

In Commix, systems are declared as a data structures, typically loaded from
an [edn][] resource. In Commix (unlike Component) any Clojure data structure can
be a component and anything can depend on anything else.

In Commix (unlike Integrant) systems and components are Clojure maps which could
be grouped in arbitrary hierarchies within other systems. There is no
distinction between systems and components. Any valid system can be reused as a
component within other system allowing for simple module-like semantics. Also
unlike with Integrant, in Commix all life-cycle actions return system maps which
can be pipelined into other action.

Like with other systems each component in Commix accesses its dependencies as
parameters in a single level map. But declaration of the components' behavior,
component's structure and system's topology is considerably different.

In Component and Mount declaration of components is distributed and there is no
centralized definition of the system. In Integrant, declaration of the
life-cycle behavior of components is distributed, but declaration of the system
is a monolithic data structure and must be defined in one place. In Commix both
behavior and structure can be arbitrarily distributed. You have the freedom to
define your system in one place or split it in multiple sub-systems.

In Commix life-cycle methods can be invoked on parts of the system. Methods need
not be idempotent and order of the methods is restricted by a transition
matrix. For example, actions like `init` and `halt` will never run twice in a
row on the same component and will fail after actions which they were not
designed to follow. Custom life-cycle actions are very easy to write.

Commix doesn't lose references. Errors during life-cycle phases are caught and
passed to the exception handler. A valid system is always returned to the caller
for further manipulation or `halt`.

[edn]: https://github.com/edn-format/edn

## Installation

To install, add the following to your project `:dependencies`:

    ;; not on clojars yet, still in early alpha stage
    [commix "0.1.0-ALPHA"]


## Usage

### Configuring and Running Systems at a Glance

Commix components and systems are maps where each key is a parameter, component
or a reference to a component.

```clojure
{:parameter 1 ; <- plain parameter

 :com-A (cx/com :ns1/prototype-A
          {:param1 10                     ; <- plain parameter
           :param2 (cx/ref :parameter)})  ; <- :parameter is a dependence

 :com-B (cx/com :ns1/prototype-B
          {:foo ""                        ; <- plain parameter
           :bar (cx/ref :parameter)       ; <- root's [:parameter] is a dependence
           :baz (cx/ref :com-A)           ; <- root's [:com-A] is a dependence
           :qux (cx/com :ns2/prototype2   ; <- [:com-B :qux] :is a dependency of [:com-B]!
                  {:a 1, :b 2, ,,,})
 }
```

Configuration maps specify system topology and dependency relationships across
parameters and components. Plain parameters can be any Clojure data
structures. Components are declared with [`cx/com`](#specifying-components).
References are keys or vectors of keys marked with [`cx/ref`](#specifying-references).

Life-cycle behaviors are encapsulated in prototype keys (`:ns/prototype-A`,
`:ns/prototype-B`, `:ns2/prototype2`) through multimethods which dispatch on
these keys - `init-com`, `halt-com` etc. See [Life-cycle Methods][#life-cycle-methods-init-com-halt-com-etc].


```clojure
(defmethod cx/init-com :ns1/prototype-A [{:keys [param1 param2 param3] :as config} _]
  (do-something-with param1 param2 param3))

(defmethod cx/halt-com :ns1/prototype-A [_ v]
  (stop-resources v))
```

The value returned by life-cycle methods is substituted into the system map,
ready to be passed to the next life-cycle method.

_A Simple Example:_

```clojure

;; 1) Define Behavior:

(require '[commix.core :as cx])

(defmethod cx/init-com :timer/periodically [{:keys [timeout action]}]
  (let [now   #(quot (System/currentTimeMillis) 1000)
        start (now)]
    (future (while true
              (Thread/sleep timeout)
              (action (- (now) start))))))

(defmethod cx/halt-com :timer/periodically [{v :cx/value}]
  (future-cancel v))

;; 2) System Config:

(def config
  {
   :printer (fn [elapsed] (println (format "Elapsed %ss" elapsed)))

   :reporter (cx/com :timer/periodically
               {:timeout 5000
                :action (cx/ref :printer)})
   })

;; 3) Init and Halt

(def system (cx/init config))
;; =>
;; Elapsed 5s
;; Elapsed 10s
;; ...

(cx/halt system)

```


### Specifying Components

There are 2 ways to declare components:

1. _Using `cx/com` marker_

  ```clojure
  {
   ;; prototype :ns/key with {} parameter map
   :A (cx/com :ns/key)

   ;; with config
   :B1 (cx/com :ns/key {:param 1}) ; or
   :B2 (cx/com :ns/key config)

   ;; {:param 1} is merged into config
   :C (cx/com :ns/key config {:param 1})

   ;; :cx/identity prototype which returns itself on initialization
   :D1 (cx/com {:param 1}) ; is equivalent to
   :D2 (cx/com :cx/identity {:param 1})
   }
  ```

2. _Using Integrant's syntax_

  ```clojure
  {
   ;; A namespaced parameter followed by a map is considered a component.
   ::D1 {:param 1}
   ;; is equivalent to
   ::D1 (cx/com ::D1 {:param 1})
   }
  ```

Note that the above configurations are valid [edn][]. Commix expands `cx/com`
markers in quoted lists as if `cx/com` was called directly. Symbols in the
second argument to `cx/com` are resolved to their value. The following
statements are equivalent:

```clojure
(def com-config {:param 1})

(cx/init {:A (cx/com :ns/name {:param 1})})
(cx/init '{:A (cx/com :ns/name {:param 1})})
(cx/init '{:A (cx/com :ns/name com-config)})
(cx/init {:A '(cx/com :ns/name {:param 1})})
(cx/init {:A '(cx/com :ns/name com-config)})
(cx/init {:A (cx/com :ns/name 'com-config)})
(cx/init (read-string "{:A (cx/com :ns/name {:param 1})}"))
(cx/init (read-string "{:A (cx/com :ns/name com-config)}"))
(cx/init (edn/read-string "{:A (cx/com :ns/name com-config)}"))
```

### Specifying References

References are marked with `cx/ref`. Argument to `cx/ref` is a key or vector of
keys referring to parameters or components within the configuration map.

References abide by following rules:

  1. Lookup is done from `cx/ref` outwards.
  2. `cx/com` boundaries are invisible, as if `cx/coms` are maps (which they
     are after the expansion).
  3. Matched sub-path need not start at root of the config map.
  4. References cannot enter non-parent components.


```clojure
(def config
  {
   :pars {:par 1 :bar 2}
   :gr   {:grsys1 (cx/com {:foo 1})
          :grsys2 (cx/com {:bar 2})}
   :sys  (cx/com {:foo 1 :bar 1})
   :sys1 (cx/com
           {:pars2 {:a 1 :b 2}
            :quax  (cx/com {})
            :s     (cx/com
                     {
                      :a (cx/ref :quax)         ; refers to [:sys1 :quax]
                      :b (cx/ref [:sys1 :quax]) ; same
                      :c (cx/ref :sys)          ; ref to component from root
                      :d (cx/ref [:gr :grsys1]) ; ref to component from root
                      :e (cx/ref [:pars2])      ; refers to [:sys1 :pars2] parameter
                      :f (cx/ref [:pars :par])  ; [:pars :par] parameter
                      :g (cx/ref [:pars2 :a])   ; [:sys1 :pars2 :a] parameter
                      })
            :t     (cx/com
                     {
                      :a (cx/ref [:s :a])     ; invalid, cannot refer in non-parent!
                      :b (cx/ref [:sys :foo]) ; invalid, cannot refer in non-parent!
                      :d (cx/ref [:gr :quax]) ; invalid, cannot refer in non-parent!
                      })
            })
   })
```

Intuitively, open parenthesis in `(cx/com ,,,` is like a door - a ref can get
out and in of its own doors, but cannot enter neighbors' doors.

The above `cx/ref` semantics allows for nested subsystems. References in nested
configs will be resolved in exactly the same way as they are in top-level
configs. The following is a valid config:

```clojure
(def nested-config
  {:sub-system1 (cx/com config)
   :com         (cx/com :ns/name
                  {:x (cx/ref :sub-system1)})})
```

### Life-cycle Methods: `init-com`, `halt-com` etc.

Life cycles of components is controlled by Clojure multi-methods `init-com`,
`halt-com`, `suspend-com`, `resume-com` etc. All methods receive one argument -
a component from the system map.

Commix represents components as plain maps. Each component is the original
config map with all the `cx/refs` expanded to the initialized
dependencies. Components also contain a range of special `:cx` keys:

 - `:cx/key`    - key on which life-cyle multi-methods are dispatched
 - `:cx/value`  - value returned by the previous life-cycles method
 - `:cx/status` - action class of the last action ran on this node (`:init`, `:halt` etc.)
 - `:cx/path`   - path to the component within the system
 - `:cx/system` - whole system (use discouraged!)

```clojure
(defmethod cx/init-com :some-ns/some-key [{:keys [param1 param2 :cx/key :cx/path] :as config}]
  (do
    (something with config, param1, param2, key and path)
    ,,,
    (return initialized component)))

(defmethod cx/halt-com :default [{obj :cx/value}]
  (halt obj and return whatever you think is useful))
```

Value received in `:cx/value` slot is different for different methods, but it's
always the value which was returned by a method in previous action. For instance
`init-com` will likely receive nil if there was no other method run before
it. `halt-com` and `suspend-com` most likely receive the value returned by
`init-com`. `Resume-node` receives value returned by `syspend-node`. So,
`:cx/value` is likely to be of primary interest to all life-cycle methods except
`init-com`.

Default methods are defined for all life-cycle multi-methods except for
`init-com`. Default method for `halt-com` is identity; for `suspend-com` and
`resume-com` default methods are `halt-com` and `init-com` respectively.

While life-cycle methods can access the whole system through `:cx/system` keys,
its use is discouraged. The core idea of a "component" is that it can exist in
isolation and it is very likely that you can achieve the same effect only by an
explicit use of dependencies.

### Life-cycle Actions: `init`, `halt` etc.

All life-cycle actions (`init`, `halt`, `suspend` and `resume`) take in a system
map, optional path(s) and return a system map. Configuration stage is also part
of the life-cycle - `config == uninitialized system` and 'init' action is no
more special than any other actions.

Actions are composable:

```clojure
(-> config
    (cx/init)
    (cx/suspend [[:some :path] [:other :path]])
    (cx/resume)
    (cx/halt))
```

Each life-cycle action accepts a system and an optional collection of paths to
components. A path is a vector of keys in they system map. As a shortcut for the
most common case, if a path consists of a single key it could be specified
directly.

```clojure
(cx/halt system :a)     ; same as
(cx/halt system [:a])   ; same as
(cx/halt system [[:a]])

(cx/halt system [:a :b])     ; same as
(cx/halt system [[:a] [:b]])
```

Life-cycle methods need not be idempotent. Commix never runs same action
twice. It also fails if an action is run after a wrong action:

```clojure
(-> config
    (cx/init)
    (cx/suspend :a)
    (cx/init :a))
;; => "Wrong dependency status" Exception
```

This restriction ensures that the system cannot end up in invalid
states. Variable `cx/can-run-on-status` holds the allowed sequence of actions,
currently:

```clojure
{:init    #{nil :halt}
 :halt    #{:init :resume :suspend}
 :resume  #{:suspend}
 :suspend #{:init :resume}}
```

`Halt` can run after `init`, `resume` and `suspend`.  If you write methods for
`resume` and `suspend` make sure that your `halt` method can handle that.

Note that all actions might end operating on more nodes than those listed in the
call. For instance `init` and `resume` ensure first that all dependencies are
also on. `Halt`(`suspend`) first halt(suspend) all dependents.

This is not likely to return to the original state of the system:

```clojure
(-> system
    (cx/halt :a)
    (cx/init :a))
```

but the following will

```clojure
(-> system
    (cx/halt :a)
    (cx/init (cx/dependents system :a)))
```

#### Monitoring Life Cycles Actions

```clojure

(defmethod cx/init-com :tt/tmp [_] [:on])
(defmethod cx/halt-com :tt/tmp [_] [:stopped])
(defmethod cx/resume-com :tt/tmp [_] [:resumed])
(defmethod cx/suspend-com :tt/tmp [_] [:suspended])

(reset! #'cx/*trace-function* println)

(def config
  {:a (cx/com :tt/tmp {})
   :b (cx/com :tt/tmp
        {:b-par (cx/ref :a)})
   :c (cx/com :tt/tmp
        {:c-par (cx/ref :b)})})

(def sys
  (-> config
      (cx/init)
      (cx/suspend :b)
      (cx/resume  :b)))
;; =>
;; Running :init on [:a] (current status: null)
;; Running :init on [:b] (current status: null)
;; Running :init on [:c] (current status: null)
;; Running :suspend on [:c] (current status: :init)
;; Running :suspend on [:b] (current status: :init)
;; Skipping :resume on [:a] (current status: :init)
;; Running :resume on [:b] (current status: :suspend)

;; grouped map of components by last action
(cx/status sys)
;; => {:resume #{[:b]}, :suspend #{[:c]}, :init #{[:a]}}

;; flat map of instantiated components
(cx/icoms sys)
;; =>
;; {[:a] [:on],
;;  [:b] [:resumed],
;;  [:c] [:suspended]}

```

#### Handling Errors

By default Commix never throws. It always returns a valid system map for further
inspection or halt. Exception handling is done with `cx/*exception-handler*`
which by default pretty prints the exeption to stdout.

If you want to throw, just change the handler:


```clojure
(reset! #'cx/*exception-handler* (fn [_ ex] (throw ex)))
```

or

```clojure
(def shadow-system (atom nil))

(reset! #'cx/*exception-handler*
        (fn [system ex]
          (reset! shadow-system system)
          (throw ex)))
```

#### Extending Actions

All built-in actions are multi-methods which dispatch on `:cx/system` key within
a system map. By defining custom methods library authors can customize life
cycle methods.

```clojure
{:cx/system :some.package/xyz
 ,,,
 }
```

New custom actions are straightforward to write. See the documentation of
`def-path-action` and `run-path-action` and see the source code of the built-in action for
a self-explanatory tutorial. See also [How it works][#how-it-works] below.

All built-in life-cycle actions are graph-isomorphic in the sense that they
preserve the topology and dependency graph of the system. An action can modify
the topology, but it should be rarely needed. If it does, it should also update
`:commix.core/graph` metadata of the system.


### Reading From EDN Resources

```clojure
(-> (read-string "{:A (cx/com {:param 1})}")
    (cx/init))
```

### Loading Namespaces

It can be hard to remember to load all the namespaces that contain the relevant
multimethods. Commix can help via the `load-namespaces` function.

For example:

```clojure
(cx/load-namespaces {:foo.bar/baz {:message "hello"}})
```

will attempt to load the namespace `foo.bar` and also `foo.bar.bar`. A list of
all successfully loaded namespaces will be returned from the function. Missing
namespaces are ignored.

## How it works

System map is the expanded config map with each node additionally containing a
bunch of special keys - `:cx/key` (dispatch key), `:cx/status` (previous action
class), `:cx/value` (return value of the last life-cycle method run on this
node). Each [life-cycle action](#life-cycle-actions-init-halt-etc) runs
its [life-cycle method](#life-cycle-methods-init-com-halt-com-etc) on system
nodes in dependency order and substitutes `:cx/value` by the return value the
method. That's it.

## License

Copyright © 2017 Vitalie Spinu

Released under the MIT license.
