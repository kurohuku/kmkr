(require 'ert)
;; (require 'kmkr)
;; (require 'kr-list)


(ert-deftest test-kr:flatten ()
  (should
   (equal (kr:flatten '(1 2 (3 4) 5))
	  '(1 2 3 4 5)))
  (should
   (equal (kr:flatten '(1 (3 . 4) 5))
		      '(1 3 4 5))))

(ert-deftest test-kr:iota ()
  (should
   (equal (kr:iota 5)
	  '(0 1 2 3 4)))
  (should
   (equal (kr:iota 5 1)
	  '(1 2 3 4 5)))
  (should
   (equal (kr:iota 5 2 2)
	  '(2 4 6 8 10))))

(ert-deftest test-kr:drop ()
  (should
   (equal (kr:drop 0 '(1 2 3))
	  '(1 2 3)))
  (should
   (equal (kr:drop 1 '(1 2 3))
	  '(2 3)))
  (should
   (equal (kr:drop 4 '(1 2 3))
	  nil)))

(ert-deftest test-kr:drop-while ()
  (should
   (equal (kr:drop-while #'evenp '(1 1 2 2 3 3))
	  '(1 1 2 2 3 3)))
  (should
   (equal (kr:drop-while #'oddp '(1 1 2 2 3 3))
	  '(2 2 3 3))))

(ert-deftest test-kr:drop-until ()
  (should
   (equal (kr:drop-until #'evenp '(1 1 2 2 3 3))
	  '(2 2 3 3)))
  (should
   (equal (kr:drop-until #'oddp '(1 1 2 2 3 3))
	  '(1 1 2 2 3 3))))

(ert-deftest test-kr:take ()
  (should
   (equal (kr:take 2 '(1 2 3 4))
	  '(1 2)))
  (should
   (equal (kr:take 3 '(1 2))
	  '(1 2 nil))))

(ert-deftest test-kr:take-while ()
  (should
   (equal (kr:take-while #'evenp '(1 1 2 2 3 3))
	  nil))
  (should
   (equal (kr:take-while #'oddp '(1 1 2 2 3 3))
	  '(1 1))))

(ert-deftest test-kr:take-until ()
  (should
   (equal (kr:take-until #'evenp '(1 1 2 2 3 3))
	  '(1 1)))
  (should
   (equal (kr:take-until #'oddp '(1 1 2 2 3 3))
	  nil)))

(ert-deftest test-kr:with-collector ()
  (should
   (equal
    (kr:with-collector a
      (a 10)
      (dotimes (b 2) (a b)))
    '(10 0 1)))
  (should
   (equal
    (kr:with-collector (a)
      (a 10)
      (dotimes (b 2) (a b)))
    '((10 0 1))))
  (should
   (equal
    (kr:with-collector (a b)
      (a 10)
      (dotimes (c 2) (b c)))
    '((10) (0 1)))))




