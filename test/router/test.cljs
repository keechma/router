(ns router.test.test
  (:require [doo.runner :refer-macros [doo-tests]]
            [cljs.test :as test]
            [router.test.router]))

(doo-tests 'router.test.router)