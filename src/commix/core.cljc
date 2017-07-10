(ns commix.core
  (:refer-clojure :exclude [ref flatten])
  (:require [com.stuartsierra.dependency :as dep]
            ;; #?(:clj  [clojure.edn :as edn])
            [clojure.set :as set]
            [clojure.walk :as walk]
            ;; [clojure.string :as str]
            ))

(def ^:dynamic *trace-function* println)

(defn default-exception-handler [system ex]
  (clojure.pprint/pprint ex)
  system)

(def ^:dynamic *exception-handler* default-exception-handler)

;;; UTILS

(declare com? com-conf)

(defn- namespaced-keyword? [x]
  (and (keyword? x) (namespace x) true))

(defn- normalize-paths [paths]
  (if-not (coll? paths)
    ;; keyword
    (normalize-paths [[paths]])
    (into #{} (map #(if (vector? %) % [%]) paths))))

(defn- flatten
  "Transform a nested config into a map of [keyseq comp].
  `comp' is a config map or a Comp record."
  ([v]
   (reduce-kv flatten {} v))
  ([fmap k v]
   (let [k    (if (vector? k) k [k])
         fmap (assoc fmap k v)
         v    (if (com? v) (com-conf v) v)]
     (if-not (map? v)
       fmap
       (reduce-kv (fn [fmap k1 v1]
                    (flatten fmap (conj (vec k) k1) v1))
                  fmap
                  v)))))

(defn- system-nodes [system]
  (set/difference (dep/nodes (-> system meta ::graph))
                  #{::ROOT}))

(defn update-obj-in [system com-path f]
  (let [com     (get-in system com-path)
        new-obj (f (:cx/key com) (:cx/obj com))]
    (assoc-in system (conj com-path :cx/obj) new-obj)))

(defn system-objects [system]
  (into (sorted-map)
        (map #(vector % (get-in system (conj % :cx/obj)))
             (system-nodes system))))

(defn status [system]
  (let [coms (system-nodes system)]
    (->> coms
         (map #(vector % (get-in system (conj % :cx/status))))
         (group-by second)
         (map (fn [[k v]] [k (into (sorted-set) (map first v))]))
         (into {}))))


;;; COMs

(defn com
  "Create a component from a key or a config map."
  {:style/indent :defn}
  ([config] (cond
              (keyword? config) (com config {})              
              (map? config)     (com ::simple-component config)
              :else (throw (ex-info "Invalid config argument. Must be a map or a keyword." {:config config}))))
  ([key config]
   (cond
     (symbol? key)  (com (resolve key) config)
     (map? key)     (com ::simple-component key config)
     (keyword? key) (into (sorted-map)
                          (assoc config :cx/key key))
     :else          (throw (ex-info "Invalid key object supplied." {:key key :config config}))))
  ([key config merge-config]
   (merge (com key config) merge-config)))

(defn com-seq? [x]
  (and (seq? x) (contains? #{'cx/com 'commix.core/com} (first x))))

(defn com-map? [x]
  (and (map? x) (contains? x :cx/key)))

(defn com? [x]
  (or (com-seq? x) (com-map? x)))

(defn- com-key [x]
  (if (com-map? x)
    (:cx/key x)
    (if (com? x)
      (if (= (count x) 3)
        (second x)
        ::simple-component)
      (throw (IllegalArgumentException. (format "Invalid com: %s" x))))))

(defn- com-conf [x]
  (if (com-seq? x)
    (last x)
    (if (com-map? x)
      (dissoc x :cx/key :cx/obj)
      (throw (IllegalArgumentException. (format "Invalid com: %s" x))))))

(defn- expand-coms [config]
  (let [expand-kwds (not (-> config meta ::graph)) ; expand only on init
        expand-v    (fn [[k v]]
                      (if (and expand-kwds
                               (namespaced-keyword? k)
                               (map? v) (not (com-map? v))
                               (not= k :cx/obj))
                        (com k v)
                        (if (com-seq? v)
                          (com (com-key v) (com-conf v))
                          v)))]
    (walk/postwalk
      #(if (and (vector? %) (= (count %) 2))
         (assoc % 1 (expand-v %))
         %)
      config)))

(defn- get-coms-in-path
  "Retrieve the set of dependencies in `path'."
  [config path]
  {:pre [(vector? path)]}
  (let [v (get-in config path true)]
    (if (com? v)
      #{path}
      (cond
        (map? v) (reduce-kv (fn [c k v]
                              (if (and (keyword? k) (namespace k))
                                ;; FIXME: this one is most likely wrong
                                (conj c (conj path k))
                                (into c (get-coms-in-path config (conj path k)))))
                            #{}
                            v)
        (coll? v) (set (mapcat get-coms-in-path v))
        :else    #{}))))


;;; REFS

(defn ref
  "Create a reference to a key in a config map."
  [key]
  `(cx/ref ~key))

(defn ref?
  "Return true if its argument is a ref."
  [x]
  (and (seq? x)
       (contains? #{'cx/ref 'commix.core/ref} (first x))))

(defn- ref-key [x]
  (if (ref? x)
    (let [k (second x)]
      (if (vector? k) k [k]))
    (throw (IllegalArgumentException. (format "Invalid ref: %s" x)))))

(defn- get-refs
  "Get set of refs in object v."
  [v]
  (let [refs-in-map (fn [v]
                      (reduce-kv (fn [s k v]
                                   (if (com? v)
                                     (conj s [k])
                                     (into s (get-refs v))))
                                 #{} v))]
    (cond (ref? v)  #{(ref-key v)}
          (map? v)  (refs-in-map v)
          (coll? v) (set (mapcat get-refs v))
          :else     #{})))

(defn- all-refs
  "Take in a flat config and return a set of ref paths for each entry."
  [flat-conf]
  (into {}
        (map (fn [[k v]] [k (get-refs v)])
             flat-conf)))


;;; DEPS

(defn- dep-path-of-a-ref
  "Get full dependency path of a ref."
  [config com-path ref-path]
  {:pre [(vector? ref-path)]}
  (loop [cp com-path]
    (let [rk (into (vec cp) ref-path)]
      (if (get-in config rk)
        rk
        (when (seq cp)
          (recur (butlast cp)))))))

(defn- deps-from-ref
  "Get a set of full paths to dpendencies of ref key ref-path in flat map fconfig.
  com-path is the component path where ref-path was found."
  [config com-path ref-path]
  (when-let [dp (dep-path-of-a-ref config com-path ref-path)]
    (get-coms-in-path config dp)))

(defn- dependency-graph
  "Dependency graph from config."
  [config]
  (let [config (expand-coms config)
        refs   (all-refs (flatten config))]
    (reduce-kv (fn [g k v]
                 (if-not (com? (get-in config k))
                   g
                   (reduce (fn [g r]
                             (if-let [deps (deps-from-ref config k r)]
                               (reduce (fn [g d]
                                         (dep/depend g k d))
                                       g deps)
                               (throw (ex-info "Invalid reference."
                                               {:call     `(~'deps-from-ref ~'config ~k ~r)
                                                :ref-key  r
                                                :conf-key k}))))
                           ;; Need ::ROOT for tracking nodes with no deps
                           (dep/depend g k ::ROOT)
                           v)))
               (dep/graph)
               refs)))

(defn- transitive-dependencies [graph-or-sys paths reverse? exclude-self?]
  (if (satisfies? dep/DependencyGraph graph-or-sys)
    ;; graph
    (let [graph (dep/remove-all graph-or-sys ::ROOT)
          nodes (if paths
                  (let [paths  (normalize-paths paths)
                        extras (set/difference paths (dep/nodes graph))]
                    (when (seq extras)
                      (throw (IllegalAccessException.
                               (format "No component(s) in path(s): %s" extras))))
                    (->> ((if reverse?
                            dep/transitive-dependents-set
                            dep/transitive-dependencies-set)
                          graph paths)
                         (set/union (if exclude-self? #{} paths))
                         (sort (dep/topo-comparator graph))))
                  (dep/topo-sort graph))]
      (if reverse?
        (reverse nodes)
        nodes))
    ;; system
    (if (map? graph-or-sys)
      (transitive-dependencies (-> graph-or-sys meta ::graph) paths reverse? exclude-self?)
      (throw (ex-info "Invalid system. Must be a map or a dependency graph." {:supplied graph-or-sys})))))

(defn dependencies [graph-or-sys & [paths exclude-self?]]
  (transitive-dependencies graph-or-sys paths false exclude-self?))

(defn reverse-dependencies [graph-or-sys & [paths exclude-self?]]
  (transitive-dependencies graph-or-sys paths true exclude-self?))

(def dependents reverse-dependencies)



;;; KEYED GENERICS  

(defmulti init-key
  "Turn a config value associated with a key into a concrete implementation.
  In order to avoid bugs due to typos there is no default method for
  `init-key'."
  {:arglists '([key value])}
  (fn [key value] key))

(declare init)
(defmethod init-key ::simple-component [_ v] v)
(defmethod init-key ::system [_ config] (init config))


(defmulti halt-key
  "Halt a running or suspended implementation associated with a key.
  This is often used for stopping processes or cleaning up resources. 

  As with other key methods the return of `halt-key' is inserted into the system
  map, this could be used for statistics reports and inspection of the stopped
  components. If return value is nil the original component configuration will
  be inserted into the system map.

  This multimethod must be idempotent in the sense that if the component is
  already stopped this method shouldn't throw. In case system map already
  contains in this slot a configuration object, this methods is not
  called. Thus, if this method returns nil, it's automatically idempotent.

  Default method is a nop."
  {:arglists '([key value])}
  (fn [key value] key))

(defmethod halt-key :default [_ v])

(defmulti resume-key
  "Turn a config value associated with a key into a concrete implementation,
  but reuse resources (e.g. connections, running threads, etc). By default this
  multimethod calls init-key."
  {:arglists '([key value])}
  (fn [key value] key))

(defmethod resume-key :default [k v]
  (init-key k v))

(defmulti suspend-key
  "Suspend a running implementation associated with a key, so that it may be
  eventually passed to resume-key. By default this multimethod calls halt-key!,
  but it may be customized to do things like keep a server running, but buffer
  incoming requests until the server is resumed."
  {:arglists '([key value])}
  (fn [key value] key))

(defmethod suspend-key :default [k v]
  (halt-key k v))


;;; ACTIONS

(def ^{:doc "When action :A is performed on a component, all the component's
dependencies must have their status in (:A
@required-dependent-status). Exception is thrown if this condition is not
satisfied."}
  required-dependency-status
  (atom {:init #{:init :resume}
         :resume #{:init :resume}
         :halt #{:ALL}
         :suspend #{:ALL}
         }))

(def ^{:doc "When action :A is performed on a component, all the component's
dependents must have their status in (:A @required-dependent-status). Exception
is thrown if this condition is not satisfied."}
  required-dependent-status
  (atom {:init #{:ALL}
         :resume #{:ALL}
         :halt #{:halt nil}
         :suspend #{:suspend :halt nil}
         }))

(def ^{:doc "If action :A can run after :B then :B must be in (:A
  @can-run-on-status). If this condition is not satisfied action is not
  performed (silently)."}
  can-run-on-status
  (atom {:init    #{nil :halt}
         :halt    #{:init :resume :suspend}
         :resume  #{:suspend}
         :suspend #{:init :resume}}))

(defn can-run? [system com-path action]
  (let [status (:cx/status (get-in system com-path))
        run?   (contains? (get @can-run-on-status action) status)]
    (when *trace-function*
      (*trace-function* (format "%s %s on %s (current status: %s)"
                                (if run? "Running" "Skipping")
                                action com-path status)))
    run?))

(defn check-deps-status [system com-path action reverse?]
  (let [deps     (transitive-dependencies system [com-path] reverse? true)
        required (get (if reverse?
                        @required-dependent-status
                        @required-dependency-status)
                      action)]
    (when-not (contains? required :ALL)
      (doseq [dp deps]
        (let [status (get-in system (conj dp :cx/status))]
          (when-not (contains? required status)
            (throw (ex-info (format "Wrong %s status." (if reverse? "dependent" "dependency"))
                            (merge {:com-path   com-path
                                    :action action
                                    :required-status required}
                                   (if reverse?
                                     {:dependent                 dp
                                      :dependent-status          status}
                                     {:dependency                 dp
                                      :dependency-status          status}))))))))))


(defn- action-exception [system com-path action-class ex]
  (ex-info (format "Error in %s action on component %s" action-class com-path)
           {:reason ::action-exception
            :action action-class
            :com-path com-path
            :com (get-in system com-path)}
           ex))

(defn run-action [system paths action]
  (let [exception  (atom nil)]
    (loop [completed      ()
           [path & paths] paths
           system         system]
      (if-not path
        system
        (let [system (try
                       (action system path)
                       (catch clojure.lang.ExceptionInfo ex
                         (reset! exception ex)
                         system)
                       (catch Throwable ex
                         (reset! exception (action-exception system path :unknown ex))
                         system))]
          (if @exception
            (when *exception-handler*
             (*exception-handler* system @exception))
            (recur (cons path completed) paths system)))))))

(defn- fill-obj-with-config [system com-path]
  (update-in system com-path #(assoc % :cx/obj (com-conf %))))

(defn- get-obj [system path]
  (walk/prewalk #(if (com? %) (:cx/obj %) %)
                (get-in system path)))

(defn- fill-obj-with-deps [system com-path]
  (let [filler #(walk/postwalk
                  (fn [v]
                    (if (ref? v)
                      (let [dp (dep-path-of-a-ref system com-path (ref-key v))]
                        (get-obj system dp))
                      v))
                  %)]
    (let [obj-path (conj com-path :cx/obj)
          system   (if (get-in system obj-path)
                     system
                     (fill-obj-with-config system com-path))]
      (update-in system (conj com-path :cx/obj) filler))))

(defmacro defaction [name action-class [system-arg com-path-arg] & body]
  {:pre [(keyword? action-class) (symbol? system-arg) (symbol? com-path-arg)
         (contains? @can-run-on-status action-class)]}
  ;; clojure's gensyms are valid within a syntax-quote; with nested
  ;; syntax-quotes we need to inject our own gensyms.
  (let [act-class- (gensym)
        sys-arg-   (gensym)
        path-arg-  (gensym)]
    `(defn- ~name [~system-arg ~com-path-arg]
       ~`(let [~act-class- ~action-class
               ~sys-arg-   ~system-arg
               ~path-arg-  ~com-path-arg]
           (if (can-run? ~sys-arg- ~path-arg- ~act-class-)
             (do
               (check-deps-status ~sys-arg- ~path-arg- ~act-class- false)
               (check-deps-status ~sys-arg- ~path-arg- ~act-class- true)
               ;; Reminder: nested syntax-quote is for this system-arg here
               (let [~system-arg (fill-obj-with-deps ~sys-arg- ~path-arg-)
                     ~sys-arg- (try
                                 ~@body
                                 (catch #?(:clj Throwable :cljs :default) ex#
                                     (throw (action-exception ~sys-arg- ~path-arg- ~act-class- ex#))))]
                 (assoc-in ~sys-arg- (conj ~path-arg- :cx/status) ~act-class-)))
             ~sys-arg-)))))

(defaction init-com :init [system com-path]
  (update-obj-in system com-path init-key))

(defaction halt-com :halt [system com-path]
  (update-obj-in system com-path halt-key))

(defaction suspend-com :suspend [system com-path]
  (update-obj-in system com-path suspend-key))

(defaction resume-com :resume [system com-path]
  (update-obj-in system com-path resume-key))

(defn- resume-or-init [system com-path]
  (-> system
      (resume-com com-path)
      (init-com com-path)))


;;; LIFE CYCLE

(defn init
  "Turn config into a system map.
  Keys are traversed in dependency order and initiated via the init-key."
  ([config]
   (init config nil))
  ([config paths]
   (let [config (expand-coms config)
         graph  (dependency-graph config)
         system (vary-meta config assoc ::graph graph)
         deps   (dependencies system paths)]
     (run-action system deps init-com))))

(defn halt
  "Halt a system map by applying `halt-key' in reverse dependency order."
  ([system] (halt system nil))
  ([system paths]
   (let [deps (reverse-dependencies system paths)]
     (run-action system deps halt-com))))

(defn resume
  "Resume components from preceding suspend.
  Dependencies that have been previously suspended will also be resumed,
  dependencies which were halted or never initialized will be initialized."
  ([system] (resume system nil))
  ([system paths]
   (let [deps (dependencies system paths)]
     (run-action system deps resume-com))))

(defn resume-or-init
  "Resume components from preceding suspend.
  Dependencies that have been previously suspended will also be resumed,
  dependencies which were halted or never initialized will be initialized."
  ([system] (resume system nil))
  ([system paths]
   (let [deps (dependencies system paths)]
     (run-action system deps resume-or-init))))

(defn suspend
  "Resume components from preceding suspend.
  All components that depend on this component will be also suspended."
  ([system] (suspend system nil))
  ([system paths]
   (let [deps (reverse-dependencies system paths)]
     (run-action system deps suspend-com))))

