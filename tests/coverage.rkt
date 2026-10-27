#lang racket/base
;; Enforced spec coverage.
;;
;; docs/coverage.md has one row per spec ID. This test checks that:
;;  - every ID of docs/spec.md has a row;
;;  - every ID whose status is not `todo` has at least one tagged test,
;;    (spec-test 'ID ...) or (spec-test '(... ID ...) ...), under tests/;
;;  - a row's Tests cell names exactly the files whose tests are tagged
;;    with its ID;
;;  - a `done` row names a surface form, a runtime function, and an example.
;; In strict mode (OCCUR_COVERAGE_STRICT=1, the default once the release
;; marks coverage complete) every row must be `done` and tested.
(require rackunit
         racket/file
         racket/list
         racket/path
         racket/runtime-path
         racket/string)

(define-runtime-path root "..")

(define id-rx #px"`([A-Z]{2,4}\\.[a-z][a-z.-]*)`")

(define spec-ids
  (remove-duplicates (map cadr (regexp-match* id-rx (file->string (build-path root "docs" "spec.md"))
                                              #:match-select values))))

;; rows: id -> status
(define rows
  (for*/hash ([line (file->lines (build-path root "docs" "coverage.md"))]
              [m (in-value (regexp-match #px"^\\| `([^`]+)` \\|.*\\| ([a-z]+) \\|$" line))]
              #:when m)
    (values (cadr m) (caddr m))))

(define test-files
  (for/list ([f (in-directory (build-path root "tests"))]
             #:when (equal? (path-get-extension f) #".rkt")
             ;; the helper and this file mention the syntax in comments
             #:unless (member (path->string (file-name-from-path f)) '("util.rkt" "coverage.rkt")))
    f))

;; tagged: id -> the files (as "tests/....rkt") with a test tagged with it
(define (relative f) (path->string (find-relative-path (simplify-path root) (simplify-path f))))
(define tagged
  (for*/fold ([s (hash)]) ([f test-files]
                           [m (regexp-match* #px"\\(spec-test\\s+'(\\([^)]*\\)|[A-Za-z][A-Za-z.\\-]*)"
                                             (file->string f) #:match-select cadr)])
    (for/fold ([s s]) ([id (string-split (string-trim m #px"[()]"))])
      (hash-update s id (λ (fs) (if (member (relative f) fs) fs (cons (relative f) fs))) '()))))

;; cells: id -> (section surface runtime tests example status)
(define cells
  (for*/hash ([line (file->lines (build-path root "docs" "coverage.md"))]
              [m (in-value (regexp-match #px"^[|] `([^`]+)` [|]" line))]
              #:when m)
    (values (cadr m)
            (cdr (map string-trim (string-split (substring line 1 (sub1 (string-length line))) "|" #:trim? #f))))))

(define strict?
  (or (equal? (getenv "OCCUR_COVERAGE_STRICT") "1")
      (file-exists? (build-path root "docs" ".coverage-complete"))))

(test-case "every spec ID has a row in docs/coverage.md"
  (for ([id spec-ids])
    (check-true (hash-has-key? rows id) (format "~a has no row in docs/coverage.md" id))))

(test-case "every claimed ID has a tagged test"
  (for ([(id status) (in-hash rows)] #:unless (equal? status "todo"))
    (check-true (hash-has-key? tagged id) (format "~a is ~a but no spec-test is tagged with it" id status))))

(test-case "each row names exactly the test files tagged with its ID"
  (for ([(id cs) (in-hash cells)])
    (check-equal? (length cs) 6 (format "the row of ~a does not have seven cells" id))
    (define listed (sort (filter (λ (x) (not (equal? x ""))) (map string-trim (string-split (list-ref cs 3) ","))) string<?))
    (check-equal? listed (sort (hash-ref tagged id '()) string<?) (format "the Tests cell of ~a" id))))

(test-case "a done row names a surface form, a runtime function, and an example"
  (for ([(id cs) (in-hash cells)] #:when (equal? (list-ref cs 5) "done"))
    (for ([i '(1 2 4)] [what '("surface form" "runtime function" "example")])
      (check-not-equal? (list-ref cs i) "" (format "~a is done but names no ~a" id what)))))

(test-case "tags name real spec IDs"
  (for ([id (in-hash-keys tagged)])
    (check-not-false (member id spec-ids) (format "a spec-test is tagged with ~a, which is not a spec ID" id))))

(when strict?
  (test-case "strict: every ID is done and tested"
    (for ([id spec-ids])
      (check-equal? (hash-ref rows id #f) "done" (format "~a is not done" id))
      (check-true (hash-has-key? tagged id) (format "~a has no tagged test" id)))))
