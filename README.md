# Commix

<!-- [![Build Status](https://travis-ci.org/vspinu/commix.svg?branch=master)](https://travis-ci.org/vspinu/commix) -->

> com•mix (kəˈmɪks)
>
>     to mix together, blend components


Commix is a Clojure (and ClojureScript) micro-framework for building
applications with data-driven architecture. It provide simple and idiomatic
composition of components and allows pipelines of predefined and custom
life-cycle actions.

Commix is an alternative of [Ingegrant][], [Component][] and [Mount][], and was
inspired by [Integrant][]. If you are familiar with [Integrant][] please also
have a look at [Differences with Integrant][]

[component]: https://github.com/stuartsierra/component
[mount]: https://github.com/tolitius/mount
[integrant]:https://github.com/weavejester/integrant
[Differences with Integrant]:https://github.com/vspinu/commix

## Rationale

Commix was built as a response to a range of limitations in [Integrant][] which
in turn was designed to overcome limitations of Component.

In Commix, systems are created from a configuration data structure, typically
loaded from an [edn][] resource. The architecture of the application is declared
as data, rather than code.

In Commix (unlike Component) any Clojure data structure can be a component
and anything can depend on anything else. The dependencies are resolved from the
configuration before it's initialized into a system.

In Commix (unlike Integrant) system/component declarations are simple Clojure
maps which could be grouped in arbitrary hierarchies within other systems. There
is no distinction between systems and components. Any valid system can be reused
as a components within other systems allowing for rich and re-usable
configurations. Unlike in Integrant life-cycle actions return an instance of a
system which in turn can be use in another life-cycle phase.
Se [Differences with Integrant][] for a walk through.

Commix never loses references. Errors during life-cycle phases are caught and
passed to the exception handler. A valid system is always returned to the caller
for further manipulation or `halt`.

In Commix all life-cycle methods can be invoked on parts of the system. Methods
need not be idempotent as the order of the methods is controlled by a clear
semantics of which phases are allowed to follow which. Thus, `init`, `halt`
etc. methods will never run twice in a row on the same node and will fail after
actions which they were not supposed to follow.

[edn]: https://github.com/edn-format/edn

## Installation

To install, add the following to your project `:dependencies`:

    [commix "x.y.z"]


## Usage

### Configuring and Running Systems at a Glance

Commix components and systems are maps where each key is a parameter, another
component or a reference to a component. On initialization nested components and
references are substituted and the flat map is passed to the initialization
method (`init-key`).

```clojure
{:parameter 1

 :com-A (cx/com :ns1/prototype-A
          {:param1 10                     ; <- simple parameter
           :param2 (cx/ref :parameter)})  ; <- :parameter is a dependence

 :com-B (cx/com :ns1/prototype-B
          {:foo ""                        ; <- simple parameter
           :bar (cx/ref :parameter)       ; <- :parameter is a dependence
           :baz (cx/ref :com-A)           ; <- :com-A is a dependence
           :qax (cx/com :ns2/prototype2   ; <- qax is a dependency of :com-B!
                  {:a 1, :b 2, ,,,})
 }
```

Configuration maps specify system topology and dependency relationships across
parameters and components. Life-cycle behaviors are encapsulated in prototype
keys (`:ns/prototype-A`, `:ns/prototype-B`, `:ns2/prototype2`).


```clojure
;; in ns1
(defmethod cx/init-key ::prototype-A [k {:keys [param1 param2 param3]}]
  (do-something-with param1 param2 param3))
```

The value returned by cx/init-key is substituted into the system map in place of
all dependents and references to the component. There are other life-cycle
methods - `halt-key`, `suspend-key`, `resume-key` which follow similar
multimethod protocol.

__Silly Working Example:__

```clojure

;; 1) Define Behavior:

(require '[commix.core :as cx])

(defmethod cx/init-key :timer/periodically [k {:keys [timeout action]}]
  (let [now   #(quot (System/currentTimeMillis) 1000)
        start (now)]
    (future (while true
              (Thread/sleep timeout)
              (action k (- (now) start))))))

(defmethod cx/halt-key :timer/periodically [k v]
  (future-cancel v))

;; 2) Optionaly Define Default Configs:

(def reporter-config
  {
   :timeout 1000
   :action  (fn [k e] (println "Elapsed:" e))
   ,,,
   })

;; 3) System Config:

(def config
  {
   :printer (fn [k e] (println (format "Com %s elapsed %ss" k e)))

   :reporter (cx/com
               :timer/periodically         ; <- inherit behavior from this key
               reporter-config             ; <- resolve default config from this symbol
               {
                :timeout 5000              ; <- override config with this map
                :action (cx/ref :printer)  ; <- reference parameter or component
                })
   })

;; 4) Run and halt

(def system (cx/init config))
;; =>
;; Com :timer/periodically elapsed 5s
;; Com :timer/periodically elapsed 10s
;; Com :timer/periodically elapsed 15s
;; ...
(def halted-system (cx/halt system))

```


### Specifying Components

There are 2 ways to declare components:

```clojure
{

 ;;; 1) Using cx/com marker
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

 ;;; 2) Using integrant syntax.
 ;; A namespaced parameter followed by a map is considered a component.
 ::D1 {:param 1} ; equivalent to
 ::D1 (cx/com ::D1 {:param 1})

 }
```

Note that the above configuration is a valid [edn][] statement. You can read it
from file or quote and Commix `init` action will do the right thing. Commix
understands `cx/com` markers in quoted lists and expands those as if `cx/com`
was actually called directly.

Symbols as a second argument to `cx/com` (`config` above) are automatically
resolved to their value. The following are equivalent

```clojure
(def com-config {:param 1})

(cx/init {:A (cx/com :ns/name {:param 1})})
(cx/init '{:A (cx/com :ns/name {:param 1})})
(cx/init '{:A (cx/com :ns/name com-config)})
(cx/init {:A '(cx/com :ns/name {:param 1})})
(cx/init {:A '(cx/com :ns/name com-config)})
(cx/init {:A (cx/com :ns/name 'com-config)})
(cx/init (read-string "{:A (cx/com :ns/name {:param 1})}"))
```

### Specifying References

References are specified with `cx/ref`. Argument to `cx/ref` can be a key or
vector of keys referring to parameters within the configuration map.

References abide by following rules:

  1. Reference lookup is done from inside out (towards the root).
  2. `cx/com` boundaries are invisible, as if `cx/coms` are maps (they actually
     are after the expansion)
  3. Matched sub-path need not start at root of the config map.
  4. References cannot enter non-parent components


```clojure
(def config
  {
   :pars {:par 1 :bar 2}
   :gr   {:grsys1 (cx/com {:foo 1})
          :grsys2 (cx/com {:bar 2})}
   :sys  (cx/com {:foo 1 :bar 1})
   :sys1 (cx/com
           {:par  4
            :quax (cx/com {})
            :s    (cx/com
                    {
                     :a (cx/ref :quax)         ; refers to [:sys1 :quax]
                     :b (cx/ref [:sys1 :quax]) ; same
                     :c (cx/ref :sys)          ; ref to component
                     :d (cx/ref [:gr :grsys1]) ; ref component
                     :e (cx/ref [:par])        ; refers to [:sys1 :par] parameter
                     :f (cx/ref [:pars :par])  ; [:pars :par] parameter
                     })
            :t    (cx/com
                    {
                     :a (cx/ref [:s :a])     ; invalid, cannot refer to non-parent!
                     :b (cx/ref [:sys :foo]) ; invalid, cannot refer to non-parent!
                     :d (cx/ref [:gr :quax]) ; invalid, cannot refer to non-parent!
                     })
            })
   })
```

The above rules could be summarized intuitively as follows. Open parenthesis in
`(cx/com ,,,)` is like a door - a ref can get out of its own door but cannot
open and enter strangers' doors.

This reference semantics allows for composition of configurations and nesting of
subsystems. References in nested configs will be expanded exactly the same way
as they are in stand-alone configs. The following would be a valid config:

```clojure
(def nested-config
  {:sub-system1 (cx/com config)
   :com         (cx/com :ns/name
                  {:x (cx/ref :sub-system1)})})
```


### Life-cycle Actions: init, halt etc.

All built-in life-cycle actions (`init`, `halt`, `suspend` and `resume`) take in
a system map and return a system map. Configuration is a valid stage in system's
life-cycle - it's an un-initialized system map.

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

This restriction ensures that invalid system states can never occur. Variable
`cx/can-run-on-status` holds the allowed sequence of actions, currently:

```clojure
{:init    #{nil :halt}
 :halt    #{:init :resume :suspend}
 :resume  #{:suspend}
 :suspend #{:init :resume}}
```

`Halt` can run after `init`, `resume` and `suspend`. Thus, if you write methods
for `resume` and `suspend` make sure that your `halt` method can handle that.

Note that all actions might end operating on more nodes than those listed in the
call. For instance `init` and `resume` ensure furst that all dependencies are
running. `Halt` and `suspend` first halt or suspend all dependents. The
following is not likely to result in the original state:

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

#### Monitoring Life Cycles

```clojure

(defmethod cx/init-key :tt/tmp [k v] [:on])
(defmethod cx/halt-key :tt/tmp [k v] [:stopped])
(defmethod cx/resume-key :tt/tmp [k v] [:resumed])
(defmethod cx/suspend-key :tt/tmp [k v] [:suspended])

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

(cx/status sys)
;; => {:resume #{[:b]}, :suspend #{[:c]}, :init #{[:a]}}

(cx/objects sys)
;; =>
;; {[:a] [:on],
;;  [:b] [:resumed],
;;  [:c] [:suspended]}

```

#### Handling Errors

By default Commix never throws. It always returns a valid system map which you
can inspect or halt if needed. Exception handling is done with
`cx/*exception-handler*` which by default pretty prints the exeption to stdout.

If you want to throw like other life-cycle frameworks do, just reset the handler:


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

TODO: Not implemented yet.

## How it works

After initialization the system map the original config map with special keys
(`:cx/obj`,`:cx/status`) inserted into each component. Each action runs through
the system's nodes in specified dependency order and updates `:cx/obj` within
each component.

All default actions are isomorphic - they preserve the topology and dependency
graph of the system. This need not be the case in general. Custom actions are
straightforward to write. Use package source as a self-explanatory tutorial.

## License

Copyright © 2017 Vitalie Spinu

Released under the MIT license.
