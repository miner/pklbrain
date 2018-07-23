(ns miner.test-pklbrain
  (:require [clojure.test :refer :all]
            [miner.pklbrain :refer :all]))


(deftest show-info
  (testing "Show test info"
    (println)
    (println "  ** Test Info **")
    (println "  PKL Brain" (nth (clojure.edn/read-string (slurp  "project.clj")) 2))
    (println "  Clojure" (clojure-version))
    (println)
    true))

