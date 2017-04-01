#!/usr/bin/env racket

;; Web Search Utility for rofi.
;;
;; Lets you quickly launch web searches from rofi. Call it with
;;
;; $ rofi -show web:./web_rofi.sh
;;
;; A `web-entry` defines a web search. It is made of a
;; - key: The keyboard shortcut to use in rofi.
;; - description: A short explanation of the entry.
;; - $url: The url to call templated with the "{query}" argument.
;;
;; Example with Qwant search engine.
;; (web-entry "qwant"  "Search Web for {query}"  "https://www.qwant.com/?q={query}")
;;
;; In rofi window: first argument should be the key of your web entry.
;; Last argument should be the query.
;;
;; Edit `web-entries` list to add custom web searches.

#lang racket/base

(require racket/format
         racket/string
         net/sendurl)

;; Web Search Entry
(struct web-entry (key description $url))

;; Lookup into a list of web-entry `ws' the web-entry with the key `k'
;;
;; wentry-lookup: [web-entry] → string → web-entry
(define (wentry-lookup ws k)
  (car (filter (λ (w) (equal? k (web-entry-key w))) ws)))

;; Displays web-entries on standard output
;;
;; wentry-display-candidates: [web-entry] → IO string
(define (wentry-display-candidates ws)

  ;; Size of the longest key
  (define longest-key
    (foldr max 0 (map (compose string-length web-entry-key) ws)))

  ;; Size of the longest description
  (define longest-desc
    (foldr max 0 (map (compose string-length web-entry-description) ws)))

  ;; Formats web-entry to print key and description
  ;;
  ;; wentry-key-and-desc: web-entry → string
  (define (wentry-key-and-desc w)
    (define padded-key (~a (web-entry-key w)
                           #:min-width longest-key
                           #:right-pad-string " "))
    (define padded-desc (~a (web-entry-description w)
                            #:min-width longest-desc
                            #:right-pad-string " "))
    (format "~a ~s " padded-key padded-desc))

  (for-each displayln (map wentry-key-and-desc ws)))

;; List of all web search entries
(define web-entries
  (list
   (web-entry "qwant"  "Search Web for {query}"     "https://www.qwant.com/?q={query}")
   (web-entry "cnrtl"  "French {query} definition"  "http://cnrtl.fr/definition/{query}")
   (web-entry "conjug" "French {query} conjugaison" "http://cnrtl.fr/morphologie/{query}")))

(cond
  ;; Display the list of web search entries when no arguments
  [(eq? 0 (vector-length (current-command-line-arguments)))
   (wentry-display-candidates web-entries)]
  ;; Get the key and the query value and call the browser
  [else
   (let* ([args (string-split (vector-ref (current-command-line-arguments) 0))]
          [key (list-ref args 0)]
          [query (list-ref args (sub1 (length args)))]
          [w (wentry-lookup web-entries key)]
          [$url (web-entry-$url w)]
          [url (string-replace $url "{query}" query)])
     ;; TODO: Give browser the focus
     (send-url url #:escape? #t))])
