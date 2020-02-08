(defpackage lidle/tests/main
  (:use :cl
        :lidle
        :rove))
(in-package :lidle/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :lidle)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
