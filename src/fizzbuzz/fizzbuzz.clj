(ns fizzbuzz.fizzbuzz)

;; The following is mostly based on the 6th chapter of Tom Stuart's book
;; "Understanding Computation" (O'Reilly, 2013), where he uses Ruby to implement
;; the classic FizzBuzz algorithm in a Church-encoded version.

;; This port closely follows the steps presented on pages 162-192, with minor
;; differences and additions (for example, naming the functions according to
;; Clojure/Lisp conventions, or using ASCII values for character
;; representation).


;; ---------------------------------------------------
;; The FizzBuzz algorithm as a regular Clojure program
;; ---------------------------------------------------

;; This is a naive implementation of course, but suits our purposes here.
(defn fizzbuzz-template []
  (map
    (fn [n]
      (if (zero? (mod n 15))
        "FizzBuzz"
        (if (zero? (mod n 5))
          "Fizz"
          (if (zero? (mod n 3))
            "Buzz"
            (str n)))))
    (range 1 101)))


;; -----------------------------------
;; Implementation with Church encoding
;; -----------------------------------

;; We will implement the above program in (a Clojure emulation of the) lambda
;; calculus, that is, using nothing but one-argument anonymous function
;; definitons/calls, and reference to function arguments (that themselves can
;; only be anonymous functions)! (This is the so-called Church encoding of
;; operations and data.)

;; For the sake of readability, we allow ourselves to define constant aliases
;; for the lambdas we'll come up with - that's practically the same as retyping
;; them. Also, I think there's no harm in using the familiar syntactical
;; transformation macros here and there that Clojure provides us - that's just
;; like an alternative notation, with the same semantics.

;; I'll use an "l" prefix sometimes for emphasising that the given value is a
;; Church type (meaning "implemented as lambda").

;; (In case you're not a Vim user: the triple curly braces are just fold markers
;; for the editor.)


;; -------
;; Numbers
;; -------

;; The number n would simply be an entity that "makes something happen n times".
;; That is, a function that calls another function n times on its other agument.
(def ZERO (fn [f] (fn [x] x)))
(def ONE (fn [f] (fn [x] (f x))))
(def TWO (fn [f] (fn [x] (-> x f f))))
(def THREE (fn [f] (fn [x] (-> x f f f))))
(def FOUR (fn [f] (fn [x] (-> x f f f f))))
(def FIVE (fn [f] (fn [x] (-> x f f f f f))))
(def FIFTEEN (fn [f] (fn [x] (-> x f f f f f f f f f f f f f f f))))
(def HUNDRED (fn [f] (fn [x] (-> x f f f f f f f f f f f f f f f f f f f f
                                   f f f f f f f f f f f f f f f f f f f f
                                   f f f f f f f f f f f f f f f f f f f f
                                   f f f f f f f f f f f f f f f f f f f f
                                   f f f f f f f f f f f f f f f f f f f f))))

;; Challenge: Write a Clojure macro for returning such Church numerals, for
;; convenience.


;; Inspect {{{
;; In these sections, there will be inspector functions returning native Clojure
;; values, with a couple of tests applying them. These are merely for sanity
;; checks, and not parts of the Church-encoded implementation. Here we can use
;; any Clojure features we want.

(defn ->clj-int [lnum]
  ((lnum inc) 0))

(->clj-int ZERO)  ; => 0
(->clj-int ONE)  ; => 1
(->clj-int TWO)  ; => 2
;; }}}


;; --------
;; Booleans
;; --------

(def TRUE (fn [x] (fn [y] x)))
(def FALSE (fn [x] (fn [y] y)))

;; Note that IF doesn't do anything useful, just sugar.
(def IF
  (fn [lbool] (fn [x] (fn [y]
                        ((lbool x) y)))))

;; In fact, it is just the identity fn in disguise:
(def IF (fn [lbool] lbool))


;; Inspect {{{
(defn ->clj-bool [lbool]
  ((lbool true) false))

(->clj-bool TRUE)  ; => true
(->clj-bool FALSE)  ; => false
;; }}}


;; ----------
;; Predicates
;; ----------

;; A bit of cleverness here. We call the Church numeral with an "always-false"
;; fn - so we get the FALSE result for all numbers except ZERO, the only one
;; that does not call its first argument at all (but simply returns the second
;; one, i.e. TRUE).
(def ZERO?
  (fn [lnum]
    ((lnum (fn [_] FALSE)) TRUE)))


;; Inspect {{{
(->clj-bool (ZERO? ZERO))  ; => true
(->clj-bool (ZERO? TWO))  ; => false
;; }}}


;; -----
;; Pairs
;; -----

;; We can implement our most basic data structure as a closure. The returned
;; Church pair itself is a callable lambda, as always, and its function argument
;; will tell it what to do with the enclosed data (the actual pair of values).
(def PAIR
  (fn [x] (fn [y]
            (fn [f] ((f x) y)))))

;; These are trivial: simply call the pair with a #(%1) or #(%2) fn.
(def LEFT
  (fn [lpair]
    (lpair (fn [x] (fn [y] x)))))

(def RIGHT
  (fn [lpair]
    (lpair (fn [x] (fn [y] y)))))


;; Inspect {{{
(->clj-int (LEFT ((PAIR TWO) FIVE)))  ; => 2
;; }}}


;; ------------------
;; Numeric operations
;; ------------------

;; Returns a new lnum that, when used, will first call the stored
;; (to-be-incremented) lnum with the given `f`, `x` arguments, and then call `f`
;; again on the result.
(def INCREMENT
  (fn [lnum]
    (fn [f] (fn [x] (f ((lnum f) x))))))

(def SLIDE
  (fn [lpair]
    ((PAIR (RIGHT lpair)) (INCREMENT (RIGHT lpair)))))

;; We cannot have negative values, so we put a dummy ZERO to the left slot of
;; the starting pair. It doesn't matter, since it won't be evaluated at all.
(def DECREMENT
  (fn [lnum]
    (LEFT ((lnum SLIDE) ((PAIR ZERO) ZERO)))))

(def ADD
  (fn [m] (fn [n]
            ((n INCREMENT) m))))  ; increment m, n times

(def SUBTRACT
  (fn [m] (fn [n]
            ((n DECREMENT) m))))  ; decrement m, n times

(def MULTIPLY
  (fn [m] (fn [n]
            ((n (ADD m)) ZERO))))  ; add m to ZERO, n times

(def POWER
  (fn [exp] (fn [base]
              ;; `(MULTIPLY base)` will be applied `exp` times on ONE.
              ((exp (MULTIPLY base)) ONE))))

;; Works, since SUBTRACT actually means "monus" (truncated subtraction) here.
;; (That is because DECREMENT can only give back ZERO or bigger.)
(def LESS-OR-EQUAL?
  (fn [m] (fn [n]
            (ZERO? ((SUBTRACT m) n)))))

(defn mod-template [m n]
  (if (<= n m)
    (recur (- m n) n)
    m))

(def MOD-CHEAT
  (fn [m] (fn [n]
            (((IF ((LESS-OR-EQUAL? n) m))
              ;; We need to wrap the result to defer evaluation, avoiding
              ;; infinite recursion, since our IF is not lazy! The #() macro
              ;; communicates the intent that this is just a redundant layer.
              #(((MOD-CHEAT ((SUBTRACT m) n)) n) %))
             m))))

;; This first attempt above has a problem - the function refers to itself by
;; name, inside the definition. We are used to the fact that in programming
;; languages this works automagically, but in lambda calculus, this is cheating.

;; Getting rid of self-reference with the Z combinator.

;; The Y and Z combinators are nothing short of magic, but it's not necessarily
;; simple to grok their inner workings and the ways to derive them. Here are two
;; tutorials that I have found especially helpful:
;; http://blog.tomtung.com/2012/10/yet-another-y-combinator-tutorial/
;; https://www.cs.toronto.edu/~david/courses/csc324_w15/extra/ycomb.html

;; A trivial helper function, encapsulating the self-application pattern - the
;; so-called U combinator:
(def U (fn [f] (f f)))

;; The first step is that we create a wrapper around our function (`f-maker`),
;; that takes _itself_ as an argument. This way it can refer to that value at
;; the point of recursion. With this, we avoid the problem of self-reference -
;; it's perfectly legal to refer to a bound variable. The question is, how on
;; Earth will the function get itself passed into itself?

;; This is where the Z combinator comes in, that needs this modified function to
;; apply its magic on.

;; The key thing to note here is that the `(U self)` call in `f-maker`s argument
;; will expand to _the body of Z itself_, the expression with which we have
;; started! So `f-maker` basically loads the Z combinator - and this includes
;; `f-maker` itself, since it is enclosed in Z - into the returned function,
;; providing that with the tool to recreate itself the same way.
(def Z (fn [f-maker]
         (U (fn [self]
              (f-maker #((U self) %)))))) ; here again we defer evaluation,
                                          ; avoiding infinite expansion

(def MOD (Z
           ;; MOD-maker
           (fn [self]
             (fn [m] (fn [n]
                       (((IF ((LESS-OR-EQUAL? n) m))
                         #(((self ((SUBTRACT m) n)) n) %))
                        m))))))


;; Inspect {{{
(->clj-int (INCREMENT TWO))  ; => 3
(->clj-int (DECREMENT TWO))  ; => 1

(->clj-int ((ADD ONE) TWO))  ; => 3
(->clj-int ((SUBTRACT TWO) ONE))  ; => 1
(->clj-int ((SUBTRACT ONE) TWO))  ; => 0
(->clj-int ((MULTIPLY TWO) FIVE))  ; => 10
(->clj-int ((POWER FIVE) TWO))  ; => 32

(->clj-bool ((LESS-OR-EQUAL? TWO) FIVE)) ; => true

(->clj-int ((MOD THREE) TWO))  ; => 1
(->clj-int ((MOD ((POWER THREE) THREE)) ((ADD THREE) TWO)))  ; => 2
;; }}}


;; -----
;; Lists
;; -----

;; Lists can be implemented as nested pairs.
(def EMPTY ((PAIR TRUE) TRUE))
(def CONS (fn [x] (fn [llst] ((PAIR FALSE) ((PAIR x) llst)))))
(def EMPTY? LEFT)
(def FIRST (fn [llst] (LEFT (RIGHT llst))))
(def REST (fn [llst] (RIGHT (RIGHT llst))))

;; Note: Our RANGE won't be a lazy stream here, it just builds an actual list.
(defn range-template [m n]
  (if (<= m n)
    (cons m (range-template (inc m) n))
    '()))

(def RANGE
  (Z (fn [self]
       (fn [m] (fn [n]
                 (((IF ((LESS-OR-EQUAL? m) n))
                   ;; The usual wrap because of the strict IF.
                   #(((CONS m) ((self (INCREMENT m)) n)) %))
                  EMPTY))))))

(defn foldr-template [f init ls]
  (if (empty? ls)
    init
    (f (foldr-template f init (rest ls)) (first ls))))

(def FOLDR
  (Z (fn [self]
       (fn [f] (fn [init] (fn [ls]
                            (((IF (EMPTY? ls))
                              init)
                             ;; Wrap as usual.
                             #(((f
                                 (((self f) init) (REST ls)))
                                (FIRST ls))
                               %))))))))

(defn map-template [f ls]
  ;; According to the definition of `FOLDR`, `ls'` will get the value of the
  ;; "folded rest" (when `ls` is empty, the provided `init` value), and `x` will
  ;; be `(first ls)`.
  (foldr-template (fn [ls' x] (cons (f x) ls')) '() ls))

(def MAP
  (fn [f] (fn [ls]
            (((FOLDR
                (fn [ls'] (fn [x]
                            ((CONS (f x)) ls'))))
              EMPTY)
             ls))))


;; Inspect {{{
(def one-two-three ((CONS ONE)
                    ((CONS TWO)
                     ((CONS THREE) EMPTY))))

(->clj-int (-> one-two-three FIRST))  ; => 1
(->clj-int (-> one-two-three REST FIRST))  ; => 2
(->clj-int (-> one-two-three REST REST FIRST))  ; => 3
(->clj-bool (EMPTY? one-two-three))  ; false
(->clj-bool (EMPTY? EMPTY))  ; true

(defn ->clj-list [llst]
    (if (->clj-bool (EMPTY? llst))
      '()
      (cons (FIRST llst) (->clj-list (REST llst)))))

(->> one-two-three
     ->clj-list (map ->clj-int))  ; => (1 2 3)

(def range-one-five ((RANGE ONE) FIVE))

(->> range-one-five
     ->clj-list (map ->clj-int))  ; => (1 2 3 4 5)

(->> (((FOLDR ADD) ZERO) range-one-five)
     ->clj-int)  ; => 15

(->> (((FOLDR MULTIPLY) ONE) range-one-five)
     ->clj-int)  ; => 120

(->> ((MAP INCREMENT) range-one-five)
     ->clj-list (map ->clj-int))  ; => (2 3 4 5 6)
;; }}}


;; -------
;; Strings
;; -------

;; Let's represent alphanumeric characters with their ASCII values.

(def TEN ((MULTIPLY TWO) FIVE))
(def SQUARE (POWER TWO))
(def N48 ((ADD ((POWER FIVE) TWO)) ((POWER FOUR) TWO)))  ; 0
(def N66 ((MULTIPLY (INCREMENT TEN)) (INCREMENT FIVE)))  ; B
(def N70 ((ADD N66) FOUR))  ; F
(def N105 ((ADD (SQUARE TEN)) FIVE))  ; i
(def N117 ((ADD N105) ((MULTIPLY THREE) FOUR)))  ; u
(def N122 ((ADD N117) FIVE))  ; z


;; Letters

(def CH-UPPER-B N66)
(def CH-UPPER-F N70)
(def CH-LOWER-I N105)
(def CH-LOWER-U N117)
(def CH-LOWER-Z N122)

(def FIZZ ((CONS CH-UPPER-F)
           ((CONS CH-LOWER-I)
            ((CONS CH-LOWER-Z)
             ((CONS CH-LOWER-Z) EMPTY)))))

(def BUZZ ((CONS CH-UPPER-B)
           ((CONS CH-LOWER-U)
            ((CONS CH-LOWER-Z)
             ((CONS CH-LOWER-Z) EMPTY)))))

;; This is really a piece of cake now, after so much practice.
(defn concat-template [ls1 ls2]
  (if (empty? ls1)
    ls2
    (cons (first ls1)
          (concat-template (rest ls1) ls2))))

(def CONCAT
  (Z (fn [self]
       (fn [ls1] (fn [ls2]
                   (((IF (EMPTY? ls1))
                     ls2)
                    #(((CONS (FIRST ls1))
                       ((self (REST ls1)) ls2))
                      %)))))))

(def FIZZBUZZ ((CONCAT FIZZ) BUZZ))


;; Digits

;; Like Clojure's `quot`.
(defn quot-template [m n]
  (if (<= n m)
    (inc (quot-template (- m n) n))
    0))

(def QUOT
  (Z (fn [self]
       (fn [m] (fn [n]
                 (((IF ((LESS-OR-EQUAL? n) m))
                   #((INCREMENT
                       ((self ((SUBTRACT m) n)) n))
                     %))
                  ZERO))))))

(defn to-digits-template [n]
  (let [prev-digits (if (< n 10)
                      '()
                      (to-digits-template (quot n 10)))]
    (concat prev-digits (cons (mod n 10) '()))))

(def TO-DIGITS
  (Z (fn [self]
       (fn [n]
         ((CONCAT (((IF ((LESS-OR-EQUAL? n) (DECREMENT TEN)))
                    EMPTY)
                   #((self ((QUOT n) TEN)) %)))
          ((CONS ((MOD n) TEN)) EMPTY))))))

(def TO-ASCII-DIGITS
  (fn [n]
    ((MAP (ADD N48)) (TO-DIGITS n))))

;; With TO-DIGITS expanded
#_(def TO-ASCII-DIGITS
    (fn [n']
      ((MAP (ADD N48))
       ((Z (fn [self]
             (fn [n]
                ((CONCAT (((IF ((LESS-OR-EQUAL? n) (DECREMENT TEN)))
                           EMPTY)
                          #((self ((QUOT n) TEN)) %)))
                 ((CONS ((MOD n) TEN)) EMPTY)))))
        n'))))


;; Inspect {{{
(defn ->clj-string [lchars]
  (->> lchars
       ->clj-list
       (map ->clj-int)
       (map char)
       (apply str)))

(->clj-string FIZZBUZZ)  ; => "FizzBuzz"

(->clj-int ((QUOT TWO) THREE))  ; => 0

(->> (TO-ASCII-DIGITS N105)
     ->clj-list (map ->clj-int))  ; => (49 48 53)
;; }}}


;; -------------------------------------------
;; The FizzBuzz algorithm with Church encoding
;; -------------------------------------------

(def CHURCHBUZZ
  ((MAP
    (fn [n]
      (((IF (ZERO? ((MOD n) FIFTEEN)))
        FIZZBUZZ)
       (((IF (ZERO? ((MOD n) FIVE)))
         FIZZ)
        (((IF (ZERO? ((MOD n) THREE)))
          BUZZ)
         (TO-ASCII-DIGITS n))))))
   ((RANGE ONE) HUNDRED)))

;; And voila, it works!
(doseq [x (->> CHURCHBUZZ
               ->clj-list (map ->clj-string))]
  (println x))

