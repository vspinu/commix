# Commix

<!-- [![Build Status](https://travis-ci.org/vspinu/commix.svg?branch=master)](https://travis-ci.org/vspinu/commix) -->

> com•mix (kəˈmɪks)
>
>     to mix together, blend components

Commix is a Clojure (and ClojureScript) micro-framework for building
applications with data-driven architecture. It provide simple and idiomatic
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

In Commix, systems are created from configuration data structures, typically
loaded from an [edn][] resource. The architecture of the application is declared
as data, rather than code. In Commix (unlike Component) any Clojure data
structure can be a component and anything can depend on anything else.

In Commix (unlike Integrant) system and component declarations are Clojure maps
which could be grouped in arbitrary hierarchies within other systems. There is
no distinction between systems and components. Any valid system can be reused as
a component within other system allowing for simple module-like semantics. Also
unlike with Integrant, in Commix all life-cycle actions return system maps which
can be pipelined into other action.

In Commix life-cycle methods can be invoked only on parts of the system. Methods
need not be idempotent and order of the methods is restricted by a transition
matrix. For example, actions like `init` and `halt` will never run twice in a
row on the same component and will fail after actions which they were not
designed to follow. Custom life-cycle actions are very easy to write.

Commix never loses references. Errors during life-cycle phases are caught and
passed to the exception handler. A valid system is always returned to the caller
for further manipulation or `halt`.

[edn]: https://github.com/edn-format/edn

## Installation

To install, add the following to your project `:dependencies`:

    [commix "x.y.z"]


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
keys - `init-key`, `halt-key` etc. See [Life-cycle Methods][#life-cycle-methods-init-key-halt-key-etc].


```clojure
(defmethod cx/init-key :ns1/prototype-A [{:keys [param1 param2 param3] :as config} _]
  (do-something-with param1 param2 param3))

(defmethod cx/halt-key :ns1/prototype-A [_ v]
  (stop-resources v))
```

The value returned by life-cycle methods is substituted into the system map,
ready to be passed to the next life-cycle method.

_Simple Example:_

```clojure

;; 1) Define Behavior:

(require '[commix.core :as cx])

(defmethod cx/init-key :timer/periodically [{:keys [timeout action]} _]
  (let [now   #(quot (System/currentTimeMillis) 1000)
        start (now)]
    (future (while true
              (Thread/sleep timeout)
              (action (- (now) start))))))

(defmethod cx/halt-key :timer/periodically [_ v]
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
   :D1 (cx/com {:param 1}) ; equivalent to
   :D2 (cx/com :cx/identity {:param 1})
   }
  ```

2. _Using Integrant syntax_

  ```clojure
  {
   ;; A namespaced parameter followed by a map is considered a component.
   ::D1 {:param 1} ; equivalent to
   ::D1 (cx/com ::D1 {:param 1})
   }
  ```  

Note that the above configurations are valid [edn][]. Commix expand `cx/com`
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

The above `cx/ref` semantics allows for nesting of subsystems. References in
nested configs will be resolved in exactly the same way as they are in
stand-alone configs. The following would be a valid config:

```clojure
(def nested-config
  {:sub-system1 (cx/com config)
   :com         (cx/com :ns/name
                  {:x (cx/ref :sub-system1)})})
```

### Life-cycle Methods: `init-key`, `halt-key` etc.

Components are instantiated from prototype keys which encapsulate life-cycle
behaviors. All methods receive two arguments, a config map and the value. 

First argument, the config map, is the original configuration of the components
with all `cx/refs` expanded to instantiated dependencies. It also contains a
range of special keys:

 - `:cx/key` - key on which life-cyle multi-methods are dispatched
 - `:cx/status` - action class of the last action ran on this node (`:init`, `:halt` etc.)
 - `:cx/path` - path to the component within the system
 - `:cx/system` - whole system (discouraged!)

While life-cycle methods can access the whole system through `:cx/system` keys,
its use is discouraged. The core idea of a "component" is that it can exist in
isolation and it's likely that you can achieve what you want with explicit use
of dependencies.

Second argument, value, is different for different methods, but it's always the
value which was returned by a method in previous action. For instance `inti-key`
will likely nil if there was no other method run before it. `halt` and `suspend`
receive an instanciated component. `Resume-key` receives value returned by
`syspend-key`.

Default methods are defined for all life-cycle multi-methods except for
`init-key`. Default method for `halt-key` is identity; for `suspend-key` and
`resume-key` default methods are `halt-key` and `init-key` respectively. 


```clojure
(defmethod cx/init-key :default [config value]
  (do
    (something with config value)
    ,,,
    (return initialized component)))

(defmethod cx/halt-key :default [config value]
  (halt value and return whatever you think is useful))
```
### Life-cycle Actions: `init`, `halt` etc.

All built-in life-cycle actions (`init`, `halt`, `suspend` and `resume`) take in
a system map and return a system map. Configuration stage is also part of the
life-cycle - `config == uninitialized system` and 'init' action is no more
special than any other actions.

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

(defmethod cx/init-key :tt/tmp [_ _] [:on])
(defmethod cx/halt-key :tt/tmp [_ _] [:stopped])
(defmethod cx/resume-key :tt/tmp [_ _] [:resumed])
(defmethod cx/suspend-key :tt/tmp [_ _] [:suspended])

(alter-var-root #'cx/*trace-function* (constantly println))

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

If you want to throw like other life-cycle frameworks, change the handler:


```clojure
(alter-var-root #'cx/*exception-handler*
                (constantly (fn [_ ex] (throw ex))))
```

or 

```clojure
(def shadow-system (atom nil))

(alter-var-root #'cx/*exception-handler*
                (constantly (fn [system ex]
                              (reset! shadow-system system)
                              (throw ex))))
```


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

System map is the original (expanded) config map with each node additionally
containing a bunch of special keys - `:cx/key` (dispatch key), `:cx/status`
(previous action class), `:cx/value` (return value of the last life-cycle method
run on this node).
 
First argument to a life-cycle method is precisely this node but with all
`cx/refs` dependencies expanded and with two extra special keys `:cx/path` (path
to this node) and `:cx/system` the whole system map.
 
All default life-cycle actions are graph-isomorphic in the sense that they
preserve the topology and dependency graph of the system. This need not be the
case in general. Custom actions are straightforward to write. See the
documentation of `defaction` and `run-action` and use default actions in the
package source as a self-explanatory tutorial.

## License

Copyright © 2017 Vitalie Spinu

Released under the MIT license.
