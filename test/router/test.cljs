(ns router.test
  (:require [doo.runner :refer-macros [doo-tests]]
            [router.router-test]
            [router.v2.router-test]))

(doo-tests
  'router.router-test
  'router.v2.router-test)