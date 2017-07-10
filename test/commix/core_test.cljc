(ns commix.core-test
  (:require [commix.core :as cx]
            [com.stuartsierra.dependency :as dep]
            [clojure.walk :as walk]
            #?(:clj  [clojure.test :refer :all]
               :cljs [cljs.test :refer-macros [deftest is testing]])))

(defn simplify-keys [config]
  (walk/postwalk #(if (#'cx/namespaced-keyword? %) (keyword (name %)) %) config))

(defmethod cx/init-key :default [k v]
  [:on])

(defmethod cx/halt-key :default [k v]
  [:stopped])

(defmethod cx/resume-key :default [k v]
  [:resumed])

(defmethod cx/suspend-key :default [k v]
  [:suspended])

(defn throw-ex-handler [system ex]
  (throw ex))

(alter-var-root #'cx/*exception-handler* (constantly throw-ex-handler))

(deftest get-refs-test
  (is (= (#'cx/get-refs {:tt (cx/ref :qq)}) #{[:qq]}))
  (is (= (#'cx/get-refs [(cx/ref :aa) (cx/ref :bb)]) #{[:aa] [:bb]}))
  (is (= (#'cx/get-refs {:a (cx/ref :quax)
                         :b (cx/ref ::sys)
                         :c (cx/ref [:gr ::grsys1]) 
                         :d (cx/ref [:gr :grsys2]) 
                         :e [(cx/ref :bbb) (cx/ref :tt) {:f [(cx/ref :aaa)]}]})
         #{[:gr :commix.core-test/grsys1] [:gr :grsys2] [:bbb] [:tt] [:aaa] [:quax]
           [:commix.core-test/sys]})))

(deftest get-deps-in-path-test
  (let [conf {:x {:z (cx/com {:a  (cx/com {})
                              :b  {:c (cx/com {})
                                   :d (cx/com {})}
                              ::d {}})
                  :y 5
                  :w (cx/com {:v {:t (cx/ref :y)}})}}]
    (is (= (#'cx/get-deps-in-path conf [:x :z :b])
           #{[:x :z :b :c] [:x :z :b :d]}))
    (is (= (#'cx/get-deps-in-path conf [:x :z])
           #{[:x :z]}))
    (is (= (#'cx/get-deps-in-path conf [:x :z :a])
           #{[:x :z :a]}))
    (is (= (#'cx/get-deps-in-path conf [:x :z ::d])
           #{}))
    (is (= (#'cx/get-deps-in-path conf [:x :w :v])
           #{}))))

(deftest expand-coms-test
  (is (= (#'cx/expand-coms
           {:a (cx/com ::abc {})
            :b (cx/com {})
            :c {:cx/key ::abc}
            ::d {}})
         {:a {:cx/key :commix.core-test/abc},
          :b {:cx/key :cx/identity},
          :c {:cx/key :commix.core-test/abc},
          :commix.core-test/d {:cx/key :commix.core-test/d}})))

(deftest namespaced-key-shortcut-test
  (let [conf-keys {
                   :pars  {:par 1 :bar 2}
                   :gr    {::grsys1 {:foo 1}
                           ::grsys2 {:bar 2}}
                   ::sys  {:foo 1 :bar 1}
                   ::sys4 {::quax {}}
                   ::sys2 {:par   4
                           ::quax {}
                           ::s    {:a (cx/ref ::quax)
                                   :b (cx/ref ::sys)
                                   :d (cx/ref [:gr])
                                   :e (cx/ref [:pars :par])
                                   }
                           }
                   ::sys3 {:tt (cx/ref :gr)}
                   }

        conf-coms {
                   :pars {:par 1 :bar 2}
                   :gr   {:grsys1 (cx/com ::grsys1
                                    {:foo 1})
                          :grsys2 (cx/com ::grsys2
                                    {:bar 2})}
                   :sys  (cx/com ::sys
                           {:foo 1 :bar 1})
                   :sys2 (cx/com ::sys2
                           {:par  4
                            :quax (cx/com ::quax {})
                            :s    (cx/com ::s
                                    {:a (cx/ref :quax)
                                     :b (cx/ref :sys)
                                     :d (cx/ref [:gr])
                                     :e (cx/ref [:pars :par])
                                     })
                            })
                   :sys3 (cx/com ::sys3
                           {:tt (cx/ref :gr)})
                   :sys4 (cx/com ::sys4
                           {:quax (cx/com ::quax {})})
                   }]
    

    (is (= (simplify-keys (#'cx/dependency-graph conf-coms))
           (simplify-keys (#'cx/dependency-graph conf-keys))))

    ;; (is (= (sort (simplify-keys (cx/init conf-coms)))
    ;;        (sort (simplify-keys (cx/init conf-keys)))))

    ;; (is (= (sort (simplify-keys (cx/halt (cx/init conf-coms))))
    ;;        (sort (simplify-keys (cx/halt (cx/init conf-keys))))))
    ))


(deftest quoted-config-equivalence-test

  (let [quote-config
        `{
          :pars {:par 1 :bar 2}
          :gr   {:grsys1 (cx/com ::grsys1
                           {:foo 1})
                 :grsys2 (cx/com ::grsys2
                           {:bar 2})}
          :sys  (cx/com ::sys
                  {:foo 1 :bar 1})
          :sys2 (cx/com ::sys2
                  {:par  4
                   :quax (cx/com ::quax {})
                   :s    (cx/com ::s
                           {:a (cx/ref :quax)
                            :b (cx/ref :sys)
                            :d (cx/ref [:gr])
                            :e (cx/ref [:pars :par])
                            })
                   })
          :sys3 (cx/com ::sys3
                  {:tt (cx/ref :gr)})
          :sys4 (cx/com ::sys4
                  {:quax (cx/com ::quax {})})}

        config {
                :pars {:par 1 :bar 2}
                :gr   {:grsys1 (cx/com ::grsys1
                                 {:foo 1})
                       :grsys2 (cx/com ::grsys2
                                 {:bar 2})}
                :sys  (cx/com ::sys
                        {:foo 1 :bar 1})
                :sys2 (cx/com ::sys2
                        {:par  4
                         :quax (cx/com ::quax {})
                         :s    (cx/com ::s
                                 {:a (cx/ref :quax)
                                  :b (cx/ref :sys)
                                  :d (cx/ref [:gr])
                                  :e (cx/ref [:pars :par])
                                  })
                         })
                :sys3 (cx/com ::sys3
                        {:tt (cx/ref :gr)})
                :sys4 (cx/com ::sys4
                        {:quax (cx/com ::quax {})})}]

    (is (= (-> config
               (cx/init)
               (cx/halt [[:sys2 :quax]])
               (cx/system-objects))
           (-> quote-config
               (cx/init)
               (cx/halt [[:sys2 :quax]])
               (cx/system-objects))
           {[:sys]        [:on],
            [:sys2]       [:stopped],
            [:sys3]       [:on],
            [:sys4]       [:on],
            [:gr :grsys1] [:on],
            [:gr :grsys2] [:on],
            [:sys2 :quax] [:stopped],
            [:sys2 :s]    [:stopped],
            [:sys4 :quax] [:on]}))

    (is (= (-> config
               (cx/init)
               (cx/halt [[:sys2 :quax] :sys4])
               (cx/system-objects))
           (-> config
               (cx/init)
               (cx/halt [[:sys2 :quax] :sys4])
               (cx/system-objects))
           {[:sys]        [:on],
            [:sys2]       [:stopped],
            [:sys3]       [:on],
            [:sys4]       [:stopped],
            [:gr :grsys1] [:on],
            [:gr :grsys2] [:on],
            [:sys2 :quax] [:stopped],
            [:sys2 :s]    [:stopped],
            [:sys4 :quax] [:on]}))
    ))

(deftest init-test

  (let [config {:a (cx/com :tt/a {})
                :b (cx/com :tt/b
                     {:c (cx/com :tt/d
                           {:d (cx/ref :a)})})}
        sys    (cx/init config)]
    
    (testing "Init"

      (is (= {:init #{[:a] [:b] [:b :c]}}
             (-> sys
                 (cx/init)
                 (cx/status))
             (-> sys
                 (cx/init)
                 (cx/init [[:a] [:b :c]])
                 (cx/status))))

      (is (= (-> config
                 (cx/init :b)
                 (cx/status))
             {:init #{[:a] [:b] [:b :c]}}))

      (is (= (-> config
                 (cx/init [[:b :c]])
                 (cx/status))
             {nil #{[:b]}, :init #{[:a] [:b :c]}}))

      (is (= (-> config
                 (cx/init :a)
                 (cx/status))
             {nil #{[:b] [:b :c]}, :init #{[:a]}})))
    
    (testing "Init and Halt Interaction"
      (is (= {:init #{[:a] [:b] [:b :c]}}
             (-> sys
                 (cx/init)
                 (cx/status))
             (-> sys
                 (cx/init)
                 (cx/init [[:a] [:b :c]])
                 (cx/status))
             (-> sys
                 (cx/halt [[:b :c]])
                 (cx/init [[:b] [:b :c]])
                 (cx/status))
             (-> sys
                 (cx/halt [[:b]])
                 (cx/halt [[:b :c]])
                 (cx/init [[:b] [:b :c]])
                 (cx/status))
             (-> sys
                 (cx/halt :b)
                 (cx/init :b)
                 (cx/status))))

      (is (= {nil #{[:b] [:b :c]}, :halt #{[:a]}} 
             (-> config
                 (cx/init :a)
                 (cx/halt)
                 (cx/status)))))

    (testing "Init and Suspend Interaction"
      (is (= {:suspend #{[:a] [:b] [:b :c]}}
             (-> config
                 (cx/init)
                 (cx/suspend)
                 (cx/status))))

      (is (= {:suspend #{[:b] [:b :c]}, :init #{[:a]}} 
             (-> config
                 (cx/init)
                 (cx/suspend [[:b :c]])
                 (cx/status))))

      (is (= {nil #{[:b] [:b :c]}, :suspend #{[:a]}} 
             (-> config
                 (cx/init :a)
                 (cx/suspend)
                 (cx/status)))))))


(deftest halt-test
  (let [config {:a (cx/com :tt/a {})
                :b (cx/com :tt/b
                     {:c (cx/com :tt/d
                           {:d (cx/ref :a)})})}
        sys    (cx/init config)]
    
    (testing "Halt"
      (is (= (-> sys
                 (cx/halt :a)
                 (cx/status))
             {:halt #{[:a] [:b] [:b :c]}}))

      (is (= (-> sys
                 (cx/halt :b)
                 (cx/status))
             {:halt #{[:b]}, :init #{[:a] [:b :c]}}))

      (is (= (-> sys
                 (cx/halt [[:b :c]])
                 (cx/status))
             {:halt #{[:b] [:b :c]}, :init #{[:a]}})))
    ))


(deftest suspend-resume-test

  (let [config {:a (cx/com :tt/a {})
                :b (cx/com :tt/b
                     {:c (cx/com :tt/d
                           {:d (cx/ref :a)})})
                :e {:f (cx/com {:g (cx/ref :a)})}}
        sys    (cx/init config)]

    (def config config)
    (def sys sys)

    (testing "Init and Suspend/Resume Interaction"

      (is (= {nil #{[:b] [:b :c]}, :suspend #{[:a] [:e :f]}}
             (-> config
                 (cx/init [[:e :f]])
                 (cx/suspend)
                 (cx/status))
             (-> config
                 (cx/init [[:e :f]])
                 (cx/suspend :a)
                 (cx/status))))

      (is (= {:suspend #{[:b] [:b :c]}, :init #{[:a] [:e :f]}}
             (-> config
                 (cx/init)
                 (cx/suspend [[:b :c]])
                 (cx/status))))

      (is (= {nil #{[:b] [:b :c] [:e :f]}, :suspend #{[:a]}} 
             (-> config
                 (cx/init :a)
                 (cx/suspend)
                 (cx/status))))

      (is (thrown-with-msg?
            clojure.lang.ExceptionInfo #"^Wrong.*"
            (-> config
                (cx/init [[:e :f]])
                (cx/suspend :a)
                (cx/init [[:b :c]])
                (cx/status)))))
    
    (testing "Suspend/resume interactions"

      (is (= {:suspend #{[:b] [:e :f]}, :resume #{[:a] [:b :c]}}
             (-> sys
                 (cx/suspend :a)
                 (cx/resume [[:b :c]])
                 (cx/init :a)
                 (cx/status))))

      (is (= {:halt #{[:a] [:b] [:b :c] [:e :f]}}
             (-> sys
                 (cx/halt)
                 (cx/suspend :a)
                 (cx/status))
             (-> sys
                 (cx/halt)
                 (cx/resume)
                 (cx/status))))
      )))

(deftest independent-components-test
  (let [config {:a (cx/com :tt/a)
                :b (cx/com :tt/b)}]

    (def config config)
    (is (= (-> config
               (cx/init)
               (cx/status))
           {:init #{[:a] [:b]}}))

    (is (= (-> config
               (cx/init)
               (cx/halt :b)
               (cx/status))
           {:halt #{[:b]}, :init #{[:a]}}))

    (is (= (-> config
               (cx/init)
               (cx/halt)
               (cx/status))
           {:halt #{[:b] [:a]}}))))

(deftest nested-parameters-test
  (let [config {:a {:b (cx/com :tt/x)
                    :c (cx/com :tt/y)}
                :d (cx/com {:k [(cx/ref [:a :b])
                                (cx/ref [:a :c])]
                            :l (cx/ref :a)})}]
    (defmethod cx/init-key :tt/z [_ v]
      (vals v))
    
    (def config config)
    (is (= (-> config
               (cx/init)
               (cx/system-objects))
           {[:d]    {:k [[:on] [:on]]
                     :l {:b [:on]
                         :c [:on]}},
            [:a :b] [:on],
            [:a :c] [:on]}))))


(deftest com-expands-in-quoted-configs-test
  (let [com-config {:param 1}]

    (is (=
          (cx/init {:A (cx/com :ns/name {:param 1})})
          (cx/init '{:A (cx/com :ns/name {:param 1})})
          (cx/init '{:A (cx/com :ns/name com-config)})
          (cx/init {:A '(cx/com :ns/name {:param 1})})
          (cx/init {:A '(cx/com :ns/name com-config)})
          (cx/init {:A (cx/com :ns/name 'com-config)})
          (cx/init (read-string "{:A (cx/com :ns/name {:param 1})}"))))

    (is (=
          (#'cx/expand-coms {:A (cx/com :ns/name {:param 1})})
          (#'cx/expand-coms '{:A (cx/com :ns/name {:param 1})})
          (#'cx/expand-coms '{:A (cx/com :ns/name com-config)})
          (#'cx/expand-coms {:A '(cx/com :ns/name {:param 1})})
          (#'cx/expand-coms {:A '(cx/com :ns/name com-config)})
          (#'cx/expand-coms {:A (cx/com :ns/name 'com-config)})
          (#'cx/expand-coms (read-string "{:A (cx/com :ns/name {:param 1})}"))))))


(comment

  ;; 1) Define components:

  (require '[commix.core :as cx])

  (defmethod cx/init-key :timer/periodically [k {:keys [timeout action]}]
    (let [now   #(quot (System/currentTimeMillis) 1000)
          start (now)]
      (future (while true
                (Thread/sleep timeout)
                (action k (- (now) start))))))

  (defmethod cx/halt-key :timer/periodically [k v]
    (future-cancel v))

  ;; 2) Define and run your system

  (def reporter-config
    {
     :timeout 1000
     :action  (fn [k e] (println "Elapsed:" e))
     ,,,
     })
  
  (def config
    {
     :printer (fn [k e] (println (format "Com %s elapsed %ss" k e)))

     :reporter (cx/com
                 :timer/periodically        ; <- inherit behavior from this key
                 reporter-config            ; <- resolve default config from this symbol
                 {
                  :timeout 5000              ; <- override config with this map
                  :action  (cx/ref :printer) ; <- reference parameter or component 
                  })
     })
  
  (def system (cx/init config))
  (def halted-system (cx/halt system))
  (.beep (java.awt.Toolkit/getDefaultToolkit))
)
