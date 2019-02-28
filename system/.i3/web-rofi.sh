#!/usr/bin/env racket

;; Web Search Utility for rofi.
;;
;; Lets you quickly launch web searches from rofi. Call it with
;;
;; $ rofi -show web:./web-rofi.sh
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
;; If second argument is → then the description should follow. Rest of
;; the line consists in the query argument. See `query-args' for more
;; information.
;;
;; Edit `web-entries` list to add custom web searches.

#lang racket/base

(require racket/format
         racket/string
         racket/list
         net/sendurl)

;; Web Search Entry
(struct web-entry (key description $url))

;; Separator for the web search entry keyword and its description
(define sep "→")

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

  ;; Formats web-entry to print key and description
  ;;
  ;; wentry-key-and-desc: web-entry → string
  (define (wentry-key-and-desc w)
    (define padded-key (~a (web-entry-key w)
                           #:min-width longest-key
                           #:right-pad-string " "))

    (format "~a ~a ~a " padded-key sep (web-entry-description w)))

  (for-each displayln (map wentry-key-and-desc ws)))

;; Finds the {query} arguments in `q' for the web entry `w'. List `q'
;; could be of two form:
;;
;; > '(key → desc query-arg1 ...)
;; > '(key query-arg1 ...)
;;
;; query-args: [string] → web-entry → [string]
(define (query-args q w)
  (define desc (web-entry-description w))

  ;; Test if the list `q' contains the `sep' (ie, →). In such a case,
  ;; remove the web entry description `desc'
  (if (equal? (cadr q) sep)
      ;; Removes the "key → desc" from `q' to get the query argument
      (drop q (+ (length (string-split sep))
                 (length (string-split desc)) 1))
      ;; Only remove the "key" from `q' to get the query argument
      (drop q 1)))

;; Gets command-line arguments. Note: This function splits "normally
;; single argument" into "normally" "single" "argument"
(define (get-args)
  (string-split
   (apply string-append
          (add-between
           (append-map string-split
                       (vector->list (current-command-line-arguments)))
           " "))))

;; List of all web search entries
(define web-entries
  (list
   ;; General
   (web-entry "qwant"  "Search Web for {query}"       "https://www.qwant.com/?q={query}")
   (web-entry "wpedia" "Search Wikipedia for {query}" "https://wikipedia.org/w/index.php?search={query}")
   ;; Language Helper
   (web-entry "cnrtl"  "French {query} definition"  "http://cnrtl.fr/definition/{query}")
   (web-entry "conjug" "French {query} conjugaison" "http://cnrtl.fr/morphologie/{query}")
   (web-entry "mw"     "English {query} definition" "https://www.merriam-webster.com/dictionary/{query}")
   (web-entry "en2fr"  "Search Wordref for {query}" "http://www.wordreference.com/enfr/{query}")
   (web-entry "fr2en"  "Search Wordref for {query}" "http://www.wordreference.com/fren/{query}")
   (web-entry "lin"    "Search Linguee for {query}" "https://www.linguee.com/english-french/search?source=auto&query={query}")
   (web-entry "ox"     "Search Oxford dict for {query}" "https://www.oxfordlearnersdictionaries.com/definition/english/{query}")
   ;; Science
   (web-entry "dblp"    "CS Bib for {query}"         "http://dblp.uni-trier.de/search?q={query}")
   (web-entry "scholar" "Google Scholar for {query}" "https://scholar.google.fr/scholar?q={query}")
   (web-entry "scihub"  "Sci-Hub for {doi}"          "https://sci-hub.ac/{query}")
   ))


(cond
  ;; Display the list of web search entries when no arguments
  [(eq? 0 (length (get-args)))
   (wentry-display-candidates web-entries)]

  ;; Get the key and the query value and call the browser
  [else
   (let* ([args (get-args)]
          [key (car args)]
          [w (wentry-lookup web-entries key)]
          [$url (web-entry-$url w)]
          [query (apply string-append (add-between (query-args args w) " "))]
          [url (string-replace $url "{query}" query)])
     ;; TODO: Give browser the focus
     (send-url url #:escape? #t))])

;; Test it!
;; ./web-rofi.sh qwant Rick Astley - Never Gonna
;; ./web-rofi.sh "qwant Rick Astley - Never Gonna"
;; ./web-rofi.sh qwant → Search Web for {query} Rick Astley - Never Gonna
;; ./web-rofi.sh "qwant → Search Web for {query} Rick Astley - Never Gonna"
