(ns commix.macros)

(defmacro defaction [name action-class [system-arg com-path-arg] & body]
  {:pre [(keyword? action-class) (symbol? system-arg) (symbol? com-path-arg)]}
  `(defn- ~name [~system-arg ~com-path-arg]
     (let [action-class# ~action-class
           system-arg#   ~system-arg
           com-path-arg# ~com-path-arg]
       (if (#'commix.core/can-run? system-arg# com-path-arg# action-class#)
         (do
           (#'commix.core/check-deps-status system-arg# com-path-arg# action-class# false)
           (#'commix.core/check-deps-status system-arg# com-path-arg# action-class# true)
           (let [system-arg# (try
                               ~@body
                               (catch #?(:clj Throwable :cljs :default) ex#
                                   (throw (#'commix.core/action-exception system-arg# com-path-arg# action-class# ex#))))]
             (assoc-in system-arg# (conj com-path-arg# :cx/status) action-class#)))
         system-arg#))))
