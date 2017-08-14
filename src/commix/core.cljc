(ns commix.core
  (:refer-clojure :exclude [ref flatten eval apply])
  (:require [com.stuartsierra.dependency :as dep]
            [clojure.set :as set]
            [clojure.walk :as walk]
            [clojure.string :as str]
            #?(:clj  [clojure.pprint]
               :cljs [cljs.pprint]))
  #?(:cljs (:require-macros [commix.macros :refer [def-path-action]])
     :clj (:require [commix.macros
                     :refer [def-path-action eval apply]
                     :rename {eval eval-macro
                              apply apply-macro
                              def-path-action def-path-action-macro}])))

(defn default-exception-handler [system ex]
  #?(:clj  (clojure.pprint/pprint ex)
     :cljs (cljs.pprint/pprint ex))
  system)

(def ^:dynamic *exception-handler*
  (atom default-exception-handler))

(def ^:dynamic *trace-function*
  (atom nil))

(def ^{:doc "When action :A is performed on a component, all the component's
dependencies must have their status in (:A @required-dependent-status).
Exception is thrown if this condition is not satisfied."}
  required-dependency-status
  (atom {:init #{:init :resume}
         :resume #{:init :resume}
         :halt #{:ALL}
         :suspend #{:ALL}}))

(def ^{:doc "When action :A is performed on a component, all the component's
dependents must have their status in (:A @required-dependent-status). Exception
is thrown if this condition is not satisfied."}
  required-dependent-status
  (atom {:init #{:ALL}
         :resume #{:ALL}
         :halt #{:halt nil}
         :suspend #{:suspend :halt nil}}))

(def ^{:doc
       "If action :A can run after :B then :B must be in (:A @can-run-on-status).
If this condition is not satisfied action is not performed (silently)."}
  can-run-on-status
  (atom {:init    #{nil :halt}
         :halt    #{:init :resume :suspend}
         :resume  #{:suspend}
         :suspend #{:init :resume}}))



;;; SPECS

(def ^:private spec-available
  #?(:clj (try
            (require '[clojure.spec.alpha :as s]) true
            (catch Throwable _ false))
     :cljs false))

#?(:clj
   (do

     (defmacro spec-assert
       "Call `clojure.spec.alpha/assert` when available.
  Because speed is never a concern with life-cycle methods, set
  `clojure.spec.alpha/check-asserts` unconditionally to `true`. You can still
  disable asserts at copile time by altering `clojure.spec.alpha/*compile-asserts*`."
       [spec x]
       (if spec-available
         `(let [old-ca# (s/check-asserts?)]
            (s/check-asserts true)
            (try
              (s/assert ~spec ~x)
              (finally (s/check-asserts old-ca#))))
         `~x))

     (defmulti init-spec
       "Spec for the `init-com` input parameter."
       :cx/type)

     (defmulti halt-spec
       "Spec for the `halt-com` input parameter."
       :cx/type)

     (defmulti suspend-spec
       "Spec for the `suspend-com` input parameter."
       :cx/type)

     (defmulti resume-spec
       "Spec for the `resume-com` input parameter."
       :cx/type)

     (when spec-available

       (defmethod init-spec :default [_] (s/keys))
       (s/def :cx/init-com (s/multi-spec init-spec :cx/type))

       (defmethod halt-spec :default [_] (s/keys))
       (s/def :cx/halt-com (s/multi-spec halt-spec :cx/type))

       (defmethod suspend-spec :default [_] (s/keys))
       (s/def :cx/suspend-com (s/multi-spec suspend-spec :cx/type))

       (defmethod resume-spec :default [_] (s/keys))
       (s/def :cx/resume-com (s/multi-spec resume-spec :cx/type))

       )))


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
  "Transform a nested CONFIG into a map of elements of the form [KEYSEQ COMP]."
  ([v]
   (reduce-kv flatten {} v))
  ([fmap k v]
   (let [k    (if (vector? k) k [k])
         fmap (assoc fmap k v)
         v    (if (com? v) (com-conf v) v)]
     (if (associative? v)
       (reduce-kv (fn [fmap k1 v1]
                    (flatten fmap (conj (vec k) k1) v1))
                  fmap
                  v)
       fmap))))

(defn com-paths
  "Return paths to all components in the system map."
  [system]
  (set/difference (dep/nodes (-> system meta ::graph))
                  #{::ROOT}))

(defn values
  "Get a flat map of instantiated components (aka values) from the system map."
  [system]
  (into (sorted-map)
        (map #(vector % (get-in system (conj % :cx/value)))
             (com-paths system))))

(defn status
  "Get a grouped map of components by last run action."
  [system]
  (let [coms (com-paths system)]
    (->> coms
         (map #(vector % (get-in system (conj % :cx/status))))
         (group-by second)
         (map (fn [[k v]] [k (into (sorted-set) (map first v))]))
         (into {}))))

#?(:cljs
   ;; simple format for :cljs
   (defn format [fmt & args]
     (loop [s fmt
            [a & args] args]
       (if a
         (recur (str/replace-first s #"%s" (str a)) args)
         s))))


;;; COMs

(defn- expand-symbol [sym]
  (if (symbol? sym)
    #?(:clj (var-get (resolve sym))
       :cljs (throw (js/Error. "Cannot resolve symbols in clojurescript.")))
    sym))

(declare ref)
(defn deps
  "Expand `paths` into a map of the form {p (cx/ref p) ...} where `p` is a
  system path in `paths`."
  [& paths]
  ;; fixme: implement {:ns1 [:a :b :c]} syntax
  (reduce (fn [m p] (assoc m p (ref p)))
          {} paths))

(defn- deps-seq? [x]
  (and (seq? x) (contains? #{'cx/deps 'commix.core/deps} (first x))))

(defn- expand-deps [x]
  (if (deps-seq? x)
    (apply deps (rest x))
    x))

(defn com
  "Create a component from a key and a config map.
  All `configs` are merged. Each `config' can be a symbol, in which case it is
  automatically resolved.

  (cx/com :ns/key {:param 1})

  (cx/com :ns/key)        ; empty config
  (cx/com :ns/key config) ; from symbol

  (cx/com :ns/key config {:param 1}) ; merge {:param 1} into config

  (cx/com {:param 1})     ; is equivalent to
  (cx/com :cx/identity {:param 1})"
  {:style/indent :defn
   :arglists '([key & configs] [& configs])}
  ([& configs]
   (let [configs (->> configs
                      (map expand-symbol)
                      (map expand-deps))
         [key configs] (if (keyword? (first configs))
                         [(first configs) (rest configs)]
                         [:cx/identity configs])]
     (clojure.core/apply merge {:cx/type key} configs))))

(defn- com-seq? [x]
  (and (seq? x) (contains? #{'cx/com 'commix.core/com} (first x))))

(defn- com-map? [x]
  (and (map? x) (contains? x :cx/type)))

(defn com? [x]
  (or (com-seq? x) (com-map? x)))

(defn- com-key [x]
  (if (com-map? x)
    (:cx/type x)
    (if (com-seq? x)
      (second x)
      (throw (ex-info "Invalid com." {:object x})))))

(defn- com-conf
  "Get configuration component from a com."
  [x]
  (if (com-seq? x)
    (last x)
    (if (com-map? x)
      ;; might not be keyword if com wasn't expanded yet
      (dissoc x :cx/type :cx/value :cx/status :cx/path :cx/system)
      (throw (ex-info "Invalid com." {:object x})))))

(defn expand-com-seqs [config]
  (let [expand-kwds (not (-> config meta ::graph)) ; expand only on init
        expand-v    (fn [[k v]]
                      (if (and expand-kwds
                               (namespaced-keyword? k)
                               (map? v) (not (com-map? v))
                               (not= k :cx/value))
                        (com k v)
                        (if (com-seq? v)
                          (clojure.core/apply com (rest v))
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
      (if (associative? v)
        (reduce-kv (fn [c k v]
                     (if (and (keyword? k) (namespace k))
                       ;; FIXME: this one is most likely wrong
                       (conj c (conj path k))
                       (into c (get-coms-in-path config (conj path k)))))
                   #{}
                   v)
        #{}))))

#?(:clj
   ;; Adapted from Integrant
   (do

     (defn- keyword->namespaces [kw]
       (if-let [ns (namespace kw)]
         [(symbol ns)
          (symbol (str ns "." (name kw)))]))

     (defn- try-require [sym]
       (try (do (require sym) sym)
            (catch java.io.FileNotFoundException _)))

     (defn load-namespaces
       "Attempt to load the namespaces referenced by the keys in a configuration.
     If a key is namespaced, both the namespace and the namespace concatenated
     with the name will be tried. For example, if a key is :foo.bar/baz, then the
     function will attempt to load the namespaces foo.bar and foo.bar.baz. Upon
     completion, a list of all loaded namespaces will be returned."
       [config]
       (let [config (expand-com-seqs config)
             keys   (volatile! [])]
         (walk/postwalk (fn [v]
                          (if (com-map? v)
                            (do (vswap! keys conj (com-key v)) v)
                            v))
                        config)
         (doall (->> @keys
                     (mapcat keyword->namespaces)
                     (distinct)
                     (keep try-require)))))))


;;; REFS

(defn ref
  "Create a reference to a key in a config map."
  [key]
  `(cx/ref ~key))

(defn ref?
  "Return true if X is a `ref'."
  [x]
  (and (seq? x)
       (contains? #{'cx/ref 'commix.core/ref} (first x))))

(defn- ref-key [x]
  (if (ref? x)
    (let [k (second x)]
      (if (vector? k) k [k]))
    (throw (ex-info "Invalid ref." {:object x}))))

(defn- get-refs
  "Get set of refs in object v."
  [v & [path]]
  (let [path (or path [])
        refs-in-map (fn [v path]
                      (reduce-kv (fn [s k v]
                                   (if (com? v)
                                     (conj s (conj path k))
                                     (into s (get-refs v (conj path k)))))
                                 #{} v))]
    (cond (ref? v)  #{(ref-key v)}
          (map? v)  (refs-in-map v path)
          (coll? v) (set (mapcat get-refs v))
          :else     #{})))

(defn- all-refs
  "Take in a flat config and return a set of ref paths for each entry."
  [flat-conf]
  (into {}
        (map (fn [[k v]] [k (get-refs v)])
             flat-conf)))

;; (all-refs (flatten {:z (com {:a  (com {})
;;                              :b  {:c (com {})
;;                                   :d (com {})}
;;                              ::d {}})}))


;;; EVAL

(def ^:dynamic *allow-eval?*
  "True if evaluation of `cx/eval' forms is allowed."
  true)

#?(:clj
   (defmacro ^{:doc (:doc (meta #'eval-macro))} eval [& body]
     `(eval-macro ~@body)))

#?(:clj
   (defmacro ^{:doc (:doc (meta #'apply-macro))} apply [f & args]
     `(apply-macro ~f ~@args)))

(defn eval-form?
  "Return true if X is a `cx/eval' form."
  [x]
  (and (seq? x)
       (contains? #{'cx/eval 'commix.core/eval 'commix.macros/eval} (first x))))

(defn apply-form?
  "Return true if X is a `cx/eval' form."
  [x]
  (and (seq? x)
       (contains? #{'cx/apply 'commix.core/apply 'commix.macros/apply} (first x))))


;;; DEPS

(defn- dep-path-of-a-ref
  "Get full dependency path of a ref or nil if no dependency could be found."
  [config com-path ref-path]
  {:pre [(vector? ref-path)]}
  (loop [cp com-path]
    (let [rk (into (vec cp) ref-path)]
      (if (get-in config rk)
        rk
        (when (seq cp)
          (recur (butlast cp)))))))

(defn- deps-from-ref
  "Get a set of full paths to dpendencies of ref key REF-PATH in CONFIG.
  COM-PATH is the component path where REF-PATH was found."
  [config com-path ref-path]
  (when-let [dp (dep-path-of-a-ref config com-path ref-path)]
    (get-coms-in-path config dp)))

(defn- dependency-graph
  "Dependency graph from config."
  [config]
  (let [config (expand-com-seqs config)
        refs   (all-refs (flatten config))]
    (reduce-kv (fn [g k v]
                 (if-not (com? (get-in config k))
                   g
                   (reduce (fn [g r]
                             (if-let [deps (deps-from-ref config k r)]
                               (reduce (fn [g d]
                                         (dep/depend g k d))
                                       g deps)
                               (throw (ex-info "Missing dependency."
                                               {; :call     `(~'deps-from-ref ~'config ~k ~r)
                                                :ref-key  r
                                                :component k}))))
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
                      (throw (ex-info "No component(s) in path(s)." {:paths extras})))
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

(defn dependents [graph-or-sys & [paths exclude-self?]]
  (transitive-dependencies graph-or-sys paths true exclude-self?))


;;; KEYED METHODS

(defn- com-dispatch-fn [node]
  (or (:cx/key node)
      (throw (ex-info "Missing :cx/key in system node." {:config node}))))

(defmulti init-com
  "Turn a config value associated with a key into a concrete implementation.
  In order to avoid bugs due to typos there is no default method for
  `init-com'."
  {:arglists '([node])}
  com-dispatch-fn)

(defmethod init-com :cx/identity [node]
  ;; Not dissoccing other special keys. Otherwise would need to dissoc
  ;; recursively for other nested :cx/identity components.
  (dissoc node :cx/path :cx/system))

(defmulti halt-com
  "Halt a running or suspended node.
  Often used for stopping processes or cleaning up resources. As with other key
  methods the return of `halt-com' is inserted into the system map, this could
  be used for statistics reports and inspection of the stopped
  components. Default method is an identity."
  {:arglists '([node])}
  com-dispatch-fn)

(defmethod halt-com :default [node] (:cx/value node))

(defmulti resume-com
  "Resume a previously suspended node.
  By default calls `init-com'."
  {:arglists '([node])}
  com-dispatch-fn)

(defmethod resume-com :default [node]
  (init-com node))

(defmulti suspend-com
  "Suspend a running node, so that it may be eventually passed to resume-com.
  By default this multimethod calls halt-com, but it may be customized to do
  things like keep a server running, but buffer incoming requests until the
  server is resumed." {:arglists '([node])} com-dispatch-fn)

(defmethod suspend-com :default [node]
  (halt-com node))


;;; ACTIONS

#?(:clj
   (defmacro ^{:doc (:doc (meta #'def-path-action-macro))
               :arglists (:arglists (meta #'def-path-action-macro))}
     def-path-action [& args]
     `(def-path-action-macro ~@args)))

(defn can-run? [system com-path action]
  (let [status (:cx/status (get-in system com-path))
        run?   (contains? (get @can-run-on-status action) status)]
    (when @*trace-function*
      (@*trace-function* (format "%s %s on %s (current status: %s)"
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
                            (merge {:com-path        com-path
                                    :action          action
                                    :required-status required}
                                   (if reverse?
                                     {:dependent        dp
                                      :dependent-status status}
                                     {:dependency        dp
                                      :dependency-status status}))))))))))


(defn action-exception [system com-path action-class ex]
  (ex-info (format "Error in %s action on component %s" action-class com-path)
           {:reason ::action-exception
            :action action-class
            :com-path com-path
            :com (get-in system com-path)
            :ex ex}
           ex))

(defn run-path-action [system paths action]
  (let [exception  (atom nil)]
    (loop [completed      ()
           [path & paths] paths
           system         system]
      (if-not path
        system
        (let [system
              (try
                (action system path)
                (catch #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo) ex
                    (reset! exception ex)
                  system)
                (catch #?(:clj Throwable :cljs :default) ex
                    (reset! exception (action-exception system path :unknown ex))
                  system)
                )]
          (if @exception
            (when @*exception-handler*
             (@*exception-handler* system @exception))
            (recur (cons path completed) paths system)))))))

;; fixme: document or think of a more intuitive name
(defn- make-value [system path]
  (walk/prewalk #(if (com? %) (:cx/value %) %)
                (get-in system path)))

(defn- filled-com
  "Fill com at COM-PATH with its dependencies and eval all cx/eval and cx/apply forms."
  [system com-path]
  (let [filler #(walk/postwalk
                  (fn [v]
                    (if (ref? v)
                      (let [dp (dep-path-of-a-ref system com-path (ref-key v))]
                        (make-value system dp))
                      (if (eval-form? v)
                        (if *allow-eval?*
                          (clojure.core/eval (cons 'do (pop v)))
                          (throw (ex-info "Evaluation of `cx/eval' forms is disallowed." {:form v})))
                        (if (apply-form? v)
                          (if *allow-eval?*
                            (clojure.core/apply (second v) (pop (pop v)))
                            (throw (ex-info "Evaluation of `cx/apply' forms is disallowed." {:form v})))
                          v))))
                  %)]
    (filler (get-in system com-path))))

(defn- update-value-in [system com-path f]
  (let [node      (filled-com system com-path)
        new-value (f (assoc node
                            :cx/system system
                            :cx/path com-path))]
    (assoc-in system (conj com-path :cx/value) new-value)))

(def-path-action init-path :init [system com-path]
  (update-value-in system com-path init-com))

(def-path-action halt-path :halt [system com-path]
  (update-value-in system com-path halt-com))

(def-path-action suspend-path :suspend [system com-path]
  (update-value-in system com-path suspend-com))

(def-path-action resume-path :resume [system com-path]
  (update-value-in system com-path resume-com))

(defn- resume-or-init-path [system com-path]
  (-> system
      (resume-path com-path)
      (init-path com-path)))


;;; LIFE CYCLE ACTIONS

(defn- system-dispatch-fn [system & _]
  (:cx/system system))

(defmulti init
  "Turn config into a system map.
  Keys are traversed in dependency order and initiated via the init-com."
  {:arglists '([config] [config paths])}
  system-dispatch-fn)

(defmethod init :default
  ([config & [paths]]
   (let [config (expand-com-seqs config)
         graph  (dependency-graph config)
         system (vary-meta config assoc ::graph graph)
         deps   (dependencies system paths)]
     (run-path-action system deps init-path))))

(defmulti halt
  "Halt a system map by applying `halt-com' in reverse dependency order."
  {:arglists '([system] [system paths])}
  system-dispatch-fn)

(defmethod halt :default
  [system & [paths]]
  (let [deps (dependents system paths)]
    (run-path-action system deps halt-path)))

(defmulti resume 
  "Resume components from preceding suspend.
  Dependencies that have been previously suspended will also be resumed,
  dependencies which were halted or never initialized will be initialized."
  {:arglists '([system] [system paths])}
  system-dispatch-fn)

(defmethod resume :default
  [system & [paths]]
  (let [deps (dependencies system paths)]
    (run-path-action system deps resume-path)))

(defmulti resume-or-init
  "Resume components from preceding suspend.
  Dependencies that have been previously suspended will also be resumed,
  dependencies which were halted or never initialized will be initialized."
  {:arglists '([system] [system paths])}
  system-dispatch-fn)

(defmethod resume-or-init :default
  [system & [paths]]
  (let [deps (dependencies system paths)]
    (run-path-action system deps resume-or-init-path)))

(defmulti suspend 
  "Resume components from preceding suspend.
  All components that depend on this component will be also suspended."
  {:arglists '([system] [system paths])}
  system-dispatch-fn)

(defmethod suspend :default
  [system & [paths]]
  (let [deps (dependents system paths)]
    (run-path-action system deps suspend-path)))

