(defproject immutable-bitset "0.1.6"
  :description "a bitset which is immutable"
  :license {:name "MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies [[primitive-math "0.1.3"]]
  :global-vars {*warn-on-reflection* true}
  :profiles {:dev {:dependencies [[org.clojure/clojure "1.5.1"]
                                  [criterium "0.4.3"]
                                  [reiddraper/simple-check "0.5.6"]
                                  [collection-check "0.1.3"]]}
             :1.6 {:dependencies [[org.clojure/clojure "1.6.0"]]}
             :master {:dependencies [[org.clojure/clojure "1.7.0-alpha2"]]}}
  :test-selectors {:default (complement :benchmark)
                   :benchmark :benchmark}
  :aliases {"all" ["with-profile" "dev,1.6:dev,master:dev"]}
  :jvm-opts ^:replace ["-server"])
