#lang occur
;; A program that uses two libraries written in #lang occur.
(require "lib-a.rkt" "lib-b.rkt")
(source pings #:types (Ping) #:key dev #:from (events (ping 1 "d1") (ping 2 "d2")))
(define-stream all-pings (from pings))
(rule-template quiet #:condition (quiet-for #:seconds 60) #:applies-to none)
