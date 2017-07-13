(ns commix.test-runner
  (:require [doo.runner :refer-macros [doo-tests]]
            [commix.core-test]))

(doo-tests 'commix.core-test)
