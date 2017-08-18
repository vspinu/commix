(ns commix.core-test
  (:require [commix.core :as cx]
            [com.stuartsierra.dependency :as dep]
            [clojure.walk :as walk]
            #?(:clj  [clojure.test :refer :all]
               :cljs [cljs.test :refer-macros [deftest is testing]]))
  #?(:clj (:import [clojure.lang ExceptionInfo])))

(defn simplify-keys [config]
  (walk/postwalk #(if (#'cx/namespaced-keyword? %) (keyword (name %)) %) config))

(defmethod cx/init-com :default [{:keys [:cx/value] :as node}]
  (let [n node
        v value]
   [:on]))

(defmethod cx/halt-com :default [_]
  [:stopped])

(defmethod cx/resume-com :default [_]
  [:resumed])

(defmethod cx/suspend-com :default [_]
  [:suspended])

(reset! cx/*trace-function* nil)

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
    (is (= (#'cx/deps-from-ref conf [:x :y :w] [:x :z :b])
           #{[:x :z :b :c] [:x :z :b :d]}))
    (is (= (#'cx/deps-from-ref conf [:x :y :w] [:x :z])
           #{[:x :z]}))
    (is (= (#'cx/deps-from-ref conf [:x :y :w] [:x :z :a])
           #{[:x :z :a]}))
    (is (= (#'cx/deps-from-ref conf [:x :y :w] [:x :z ::d])
           #{}))
    (is (= (#'cx/deps-from-ref conf [:x :z] [:x :w :v])
           #{}))))

(deftest expand-com-seqs-test
  (is (= (cx/expand-com-seqs
           {:a (cx/com ::abc {})
            :b (cx/com {})
            :c {:cx/type ::abc}
            ::d (cx/com)})
         {:a {:cx/type :commix.core-test/abc},
          :b {:cx/type :cx/identity},
          :c {:cx/type :commix.core-test/abc},
          :commix.core-test/d {:cx/type :cx/identity}})))



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
               (cx/values))
           (-> quote-config
               (cx/init)
               (cx/halt [[:sys2 :quax]])
               (cx/values))
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
               (cx/values))
           (-> config
               (cx/init)
               (cx/halt [[:sys2 :quax] :sys4])
               (cx/values))
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

      (def tt (-> config
                  (cx/init [[:e :f]])
                  (cx/suspend :a)))

      ;; (cx/status tt)
      ;; (cx/init tt [[:b :c]])

      (is (thrown-with-msg? ExceptionInfo #"^Wrong.*"
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

    (defmethod cx/init-com :tt/z [node]
      (vals (:cx/value node)))

    (is (= (-> config
               (cx/init)
               (cx/values))
           {[:d]    {:k [[:on] [:on]]
                     :l {:b [:on]
                         :c [:on]}
                     :cx/type :cx/identity},
            [:a :b] [:on],
            [:a :c] [:on]}))))

(deftest deps-from-ref-is-non-recursive

  (let [conf {:a (cx/com)
              :b (cx/com
                   {:a (cx/ref :a)
                    :b (cx/ref :b)})
              :c (cx/com
                   {:a (cx/com
                         {:d (cx/ref :a)
                          :b (cx/ref :b)})})}]

    (is (= (#'cx/deps-from-ref conf [:b] [:a])
           #{[:a]}))

    (is (nil? (#'cx/deps-from-ref conf [:b] [:b])))

    (is (= (#'cx/deps-from-ref conf [:c :a] [:a])
           #{[:a]}))

    (is (= (#'cx/deps-from-ref conf [:c :a] [:b])
           #{[:b]}))

    ))

(deftest nested-vectors-of-components-initialize-correctly
  (let [config {:a (cx/com
                     {:b [(cx/com :tmp/x)
                          (cx/com :tmp/y)]})}]

    (defmethod cx/init-com :tmp/x [_] "X")
    (defmethod cx/init-com :tmp/y [_] "Y")
    (is (= (-> config
               (cx/init)
               (cx/values))
           {[:a] {:cx/type :cx/identity, :b ["X" "Y"]}
            [:a :b 0] "X"
            [:a :b 1] "Y"}))))

(deftest com-expands-deps-and-merge-configs
  (is (cx/com ::tt
        (cx/deps :a ::b [:c ::d])
        {:foo "bar"
         :baz "qax"}
        (cx/deps :x [:y])
        {:par1 1})
      {:cx/type ::tt,
       :a       (cx/ref :a),
       ::b      (cx/ref ::b),
       [:c ::d] (cx/ref [:c ::d]),
       :foo     "bar",
       :baz     "qax",
       :x       (cx/ref :x),
       [:y]     (cx/ref [:y]),
       :par1    1}))

(deftest com-expands-in-quoted-configs-test

  (def com-config {:param 1})

  #?(:clj
     (is (=
           (cx/init `{:A (cx/com :ns/name com-config)})
           (cx/init {:A `(cx/com :ns/name com-config)})
           (cx/init {:A (cx/com :ns/name `com-config)})
           (cx/init {:A (cx/com :ns/name {:param 1})})
           (cx/init '{:A (cx/com :ns/name {:param 1})})
           (cx/init {:A '(cx/com :ns/name {:param 1})})
           (cx/init (read-string "{:A (cx/com :ns/name {:param 1})}"))))
     :cljs
     (is (=
           (cx/init {:A (cx/com :ns/name {:param 1})})
           (cx/init '{:A (cx/com :ns/name {:param 1})})
           (cx/init {:A '(cx/com :ns/name {:param 1})}))))
  #?(:clj
     (is (=
           (cx/expand-com-seqs {:A (cx/com :ns/name {:param 1})})
           (cx/expand-com-seqs `{:A (cx/com :ns/name {:param 1})})
           (cx/expand-com-seqs {:A '(cx/com :ns/name {:param 1})})
           (cx/expand-com-seqs `{:A (cx/com :ns/name com-config)})
           (cx/expand-com-seqs {:A `(cx/com :ns/name com-config)})
           (cx/expand-com-seqs {:A (cx/com :ns/name `com-config)})
           #?(:clj (cx/expand-com-seqs (read-string "{:A (cx/com :ns/name {:param 1})}")))
           ))
     :cljs
     (is (=
           (cx/expand-com-seqs {:A (cx/com :ns/name {:param 1})})
           (cx/expand-com-seqs `{:A (cx/com :ns/name {:param 1})})
           (cx/expand-com-seqs {:A '(cx/com :ns/name {:param 1})})))))

(deftest modules-test

  (let [fancy-com (cx/com ::fancy
                    {:param (cx/ref :ns1/required-param)})
        config {:ns1/required-param "param"
                :fancy (cx/com fancy-com)}
        config-no-ref {:fancy (cx/com fancy-com)}]

    (is (= (-> config
               (cx/init)
               (cx/values))
           {[:fancy] [:on]}))

    (is (thrown-with-msg?
          ExceptionInfo #"^Missing dependency."
          (-> config-no-ref
              (cx/init))))))

(deftest dependency-graph-test
  (let [conf {:z (cx/com {:a  (cx/com {})
                          :b  {:c (cx/com {})
                               :d (cx/com {})}
                          ::d (cx/com)})}
        dep-graph (#'cx/dependency-graph conf)]
    (is (= (:dependencies dep-graph)
           {[:z] #{[:z :b :c] [:z :b :d] [:z :a] [:z :commix.core-test/d] :commix.core/ROOT},
            [:z :a] #{:commix.core/ROOT},
            [:z :b :c] #{:commix.core/ROOT},
            [:z :b :d] #{:commix.core/ROOT},
            [:z :commix.core-test/d] #{:commix.core/ROOT}}))
    (is (= (:dependents dep-graph)
           {:commix.core/ROOT
            #{[:z :b :c] [:z :b :d] [:z :a] [:z] [:z :commix.core-test/d]},
            [:z :b :d] #{[:z]},
            [:z :commix.core-test/d] #{[:z]},
            [:z :a] #{[:z]},
            [:z :b :c] #{[:z]}}))))

(deftest dependency-graph-with-vectors-test
  (let [conf {:x [(cx/com {:a  (cx/com)
                           :b  {:c (cx/com)
                                :d (cx/com)}
                           ::d (cx/com)})
                  (cx/com {:aa 1})]
              :y (cx/com {:aaa (cx/ref [:x 0])})
              :z (cx/com {:aaa (cx/ref [:x 1])})
              :w (cx/com {:bbb (cx/ref :x)})}
        dep-graph (#'cx/dependency-graph conf)]
    (is (= (:dependencies dep-graph)
           {[:x 0] #{[:x 0 :a] [:x 0 :b :c] [:x 0 :b :d] [:x 0 :commix.core-test/d] :commix.core/ROOT},
            [:x 0 :a] #{:commix.core/ROOT},
            [:x 0 :b :c] #{:commix.core/ROOT},
            [:x 0 :b :d] #{:commix.core/ROOT},
            [:x 0 :commix.core-test/d] #{:commix.core/ROOT},
            [:x 1] #{:commix.core/ROOT},
            [:y] #{[:x 0] :commix.core/ROOT},
            [:z] #{[:x 1] :commix.core/ROOT},
            [:w] #{[:x 0] [:x 1] :commix.core/ROOT}}))
    (is (= (:dependents dep-graph)
           {:commix.core/ROOT #{[:x 0 :commix.core-test/d] [:y] [:x 0 :b :c] [:z] [:w] [:x 0 :b :d] [:x 0] [:x 1] [:x 0 :a]},
            [:x 0] #{[:y] [:w]},
            [:x 1] #{[:z] [:w]},
            [:x 0 :b :d] #{[:x 0]},
            [:x 0 :commix.core-test/d] #{[:x 0]},
            [:x 0 :a] #{[:x 0]},
            [:x 0 :b :c] #{[:x 0]}}))))

(deftest dependency-graph-with-nested-vectors-of-components

  (let [conf {:a (cx/com
                   {:aa [(cx/com :aaa1) (cx/com :aaa2)]})}
        graph (#'cx/dependency-graph conf)]
    (is (= (:dependencies graph)
           {[:a] #{[:a :aa 1] [:a :aa 0] :commix.core/ROOT},
            [:a :aa 0] #{:commix.core/ROOT},
            [:a :aa 1] #{:commix.core/ROOT}})))

  (let [conf {:a (cx/com
                   {:a [(cx/com :aa1) (cx/com :aa2 {:aaa (cx/ref :b)})]
                    :b (cx/com {:bb1 (cx/ref :y) :bb2 (cx/ref :x)})})
              :x (cx/com)
              :y 1}
        graph (#'cx/dependency-graph conf)]
    (is (= (:dependencies graph)
           {[:x] #{:commix.core/ROOT},
            [:a] #{[:a :b] [:a :a 0] [:a :a 1] :commix.core/ROOT},
            [:a :a 0] #{:commix.core/ROOT},
            [:a :a 1] #{[:a :b] :commix.core/ROOT}
            [:a :b] #{[:x] :commix.core/ROOT},
            }))))


;;; SPECS

#?(:clj

   (cx/with-spec
     (deftest spec-validation-test
       (s/def ::boolean boolean?)
       (s/def ::string string?)
       (defmethod cx/init-spec ::comp1 [_]
         (s/keys :req [::boolean]))
       (defmethod cx/init-spec ::comp2 [_]
         (s/keys :req [::string ::boolean]))

       (let [conf {:bool true
                   :x (cx/com ::comp1
                        {::boolean true})
                   :y (cx/com ::comp1
                        {::boolean (cx/ref :bool)})}]
         (is (= :halt (-> conf cx/init cx/halt (get-in [:x :cx/status])))))

       (let [conf {:bool true
                   :y (cx/com ::comp2
                        {::boolean (cx/ref :bool)
                         ::string "string"})}]
         (is (= :halt (-> conf cx/init cx/halt (get-in [:y :cx/status])))))

       (let [conf {:bool "string"
                   :y (cx/com ::comp1
                        {::boolean (cx/ref :bool)})}]
         (is (thrown-with-msg?
               ExceptionInfo #"(?s).*Spec assertion.+:cx/init-com.*"
               (-> conf cx/init cx/halt))))

       (let [conf {:bool true
                   :y (cx/com ::comp2
                        {::boolean (cx/ref :bool)})}]
         (is (thrown-with-msg?
               ExceptionInfo #"(?s).*Spec assertion.+:cx/init-com.*"
               (-> conf cx/init cx/halt))))

       (let [conf {:bool true
                   :y (cx/com ::comp2
                        {::boolean (cx/ref :bool)
                         ::string 1111})}]
         (is (thrown-with-msg?
               ExceptionInfo #"(?s).*Spec assertion.+:cx/init-com.*"
               (-> conf cx/init cx/halt))))

       (defmethod cx/suspend-spec ::com1 [_]
         (s/keys :req [::string]))

       (let [conf {:x (cx/com ::com1
                        {::boolean true})}]
         (is (thrown-with-msg?
               ExceptionInfo #"(?s).*Spec assertion.+:cx/suspend-com.*"
               (-> conf cx/init cx/suspend))))

       )))


;;; Following Tests Overwrite :default Methods!!

(deftest value-path-status-node-test

  (defmethod cx/init-com :default [{:keys [:cx/path :cx/status :cx/value] :as node}]
    [{:path path :status status :value value}])

  (let [conf {:z (cx/com ::root
                   {:a  (cx/com ::aa {:a 1})
                    :b  {:c (cx/com :bb {:b 2})
                         :d (cx/com {})}
                    ::d (cx/com ::ddd)})}]
    ;; (cx/init conf))
    (is (= (cx/init conf)
           {:z
            {:a
             {:a 1,
              :cx/type :commix.core-test/aa,
              :cx/status :init,
              :cx/value [{:path [:z :a], :status nil, :value nil}]},
             :b
             {:c
              {:b 2,
               :cx/type :bb,
               :cx/status :init,
               :cx/value [{:path [:z :b :c], :status nil, :value nil}]},
              :d
              {:cx/type :cx/identity,
               :cx/status :init,
               :cx/value {:cx/type :cx/identity}}},
             :commix.core-test/d
             {:cx/type ::ddd
              :cx/status :init,
              :cx/value [{:path [:z :commix.core-test/d], :status nil, :value nil}]},
             :cx/type :commix.core-test/root,
             :cx/status :init,
             :cx/value [{:path [:z], :status nil, :value nil}]}}))))
