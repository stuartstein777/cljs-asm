(ns main.exfn.scratch
  (:require [clojure.string :as str]))

(let [acc {:current-section :macro
           :errors []
           :macros {}
           }])