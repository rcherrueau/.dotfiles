
(require 'cl-lib)
(require 's)
(require 'dash)
(require 'ox)
(require 'ox-html)
(require 'ox-md)

;; Use HTML5 flavor for html exporter
(setq org-html-doctype "html5")
(setq org-html-html5-fancy t)


;;; Utils

(defun slugify (str)
  "Slugify STR.

Jekyll slugify headers to attach them an id. This function
compute the slug value of STR.

See https://gist.github.com/mathewbyrne/1280286"
  (replace-regexp-in-string "\-\-+" "-"
   (replace-regexp-in-string "[^[:ascii:]]+" ""
   (replace-regexp-in-string "\\." ""
   (replace-regexp-in-string "&" "and"
   (replace-regexp-in-string "\s+" "-"
   (s-downcase (s-trim str))))))))

(defun update-last-seebling-at-depth (depth fun tree &optional nodep)
    "Returns a tree with the last seebling at depth DEPTH from
TREE replaced with `(fun tree)'.

Performs a right Depth-First-Search up until DEPTH level and
gives the subtree as argument to FUN. The FUN function then
should return the updated subtree. If there is no subtree at
depth DEPTH, then it create one.

This function assumes a TREE encoded with list, e.g.
;; '(1 (2 3) (4 (5 6) 7))
The NODEP predicate returns `t' if an element of the tree is a
node (e.g. `'(4 (5 6) 7)') or `nil' if it's a leaf (e.g. `7').
The default value of NODEP is `listp'.

Example usage, `-snoc' adds an element in the subtree:
;; (update-last-seebling-at-depth 0 (lambda (tree) (-snoc tree 1)) '(1 (2 3) (4 (5 6) 7)))
;; => '(1 (2 3) (4 (5 6) 7) 1)
;; (update-last-seebling-at-depth 1 (lambda (tree) (-snoc tree 1)) '(1 (2 3) (4 (5 6) 7)))
;; => '(1 (2 3) (4 (5 6) 7 1))
;; (update-last-seebling-at-depth 2 (lambda (tree) (-snoc tree 1)) '(1 (2 3) (4 (5 6) 7)))
;; => '(1 (2 3) (4 (5 6) 7 (1))
;; (update-last-seebling-at-depth 3 (lambda (tree) (-snoc tree 1)) '(1 (2 3) (4 (5 6) 7)))
;; => '(1 (2 3) (4 (5 6) 7 ((1)))"
    (unless nodep (setq nodep 'listp))
    (cond
     ((= depth 0)
      (funcall fun tree))
     ((> depth 0)
      (let ((last-sibling (-last-item tree)))
        (if (funcall nodep last-sibling)
            ;; The element we look for is a node: recur on it
            (-snoc (-butlast tree) (update-last-seebling-at-depth
                                    (1- depth) fun last-sibling nodep))
          ;; The element we look for is a leaf: make it a node and
          ;; recur on it
          (-snoc tree (update-last-seebling-at-depth
                       (1- depth) fun nil nodep)))))
     (t
      (error "The depth could not be smaller than 0"))))

;; Predicate for a leaf in the `toc-tree'
(defun leafp (item)
  (pcase item
    (`(,title . ,slug)
     (and (stringp title) (stringp slug)))
    (item nil)))

;; Pushes `element' in the last seebling of `tree' at depth `depth'.
(defun push-in-last-seebling (depth element tree)
  (update-last-seebling-at-depth depth
                                 (lambda (sibling) (-snoc sibling element))
                                 tree
                                 (lambda (item) (not (leafp item)))))


;;; Define Back-end

(org-export-define-derived-backend 'jekyll 'md
  :menu-entry
  '(?j "Export to Jekyll"
      ((?J "To temporary buffer"
           (lambda (a s v b) (org-jekyll-export-as-jekyll a s v)))
       (?j "To file" (lambda (a s v b) (org-jekyll-export-to-jekyll a s v)))
       (?o "To file and open"
           (lambda (a s v b)
             (if a (org-jekyll-export-to-jekyll t s v)
               (org-open-file (org-jekyll-export-to-jekyll nil s v)))))))
  :translate-alist '(
                     (code . org-html-code)
                     (src-block . org-html-src-block)
                     (link . org-html-link)
                     ;; Handle standalone images as in ox-html, handle
                     ;; the rest as in ox-md.
                     (paragraph . org-jekyll-paragraph)
                     ;; Add jekyll preamble
                     (template . org-jekyll-template)
                     ;; Support toc keyword `#+TOC: headlines 2'
                     (keyword . org-jekyll-keyword)
                     )
  :options-alist
  '((:jekyll-layout "JEKYLL_LAYOUT" nil "post" t)
    (:jekyll-categories "JEKYLL_CATEGORIES" nil "" space))
  )

(defun org-jekyll-render-toc (toc-tree)
  (pcase toc-tree
    ;; The tree is empty
    (`nil "")
    ;; The current node is a subtree
    ((and `(,tree . ,trees) (guard (not (leafp tree))))
     (s-join "\n" (list
       "<li><ul>" (s-concat (org-jekyll-render-toc tree) "</ul></li>")
       (org-jekyll-render-toc trees))))
    ;; The current node is a leaf
    ((and `(,tree . ,trees) (guard (leafp tree)))
     (cdr '("a" . "b"))
     (s-join "\n" (list
       (s-concat "<li>" (format "<a href=\"%s\">" (cdr tree)) (car tree) "</a></li>")
       (org-jekyll-render-toc trees))))
    ;; Error
    (_ (error "Unexpected toc structure for %s" toc-tree))))

(defun org-jekyll-toc (depth info &optional scope)
  "Build a table of contents.
DEPTH is an integer specifying the depth of the table.  INFO is
a plist used as a communication channel.  Optional argument SCOPE
is an element defining the scope of the table.  Return the table
of contents as a string, or nil if it is empty."
  (let* ((headlines (org-export-collect-headlines info depth scope))
         (toc-titles (-map (lambda (hd) (org-element-property :raw-value hd))
                           headlines))
         (toc-slugs  (->>
                      headlines
                      (-map (lambda (hd) (org-element-property :raw-value hd)))
                      ;; TODO: clean tags, TODO ....
                      (-map (lambda (hd) (slugify hd)))
                      (-map (lambda (hd) (s-concat "#" hd)))))
         (toc-depths (->>
                      headlines
                      (-map (lambda (h) (org-export-get-relative-level h info)))
                      (-map '1-)))
         ;; A tree like representation of the toc:
         ;; '((sec-1 . #sec-1)
         ;;   ((sec-2 . #sec-2) ((subsec-21 . #subsec-21) (subsec-22 . #subsec-22)))
         ;;   (sec-3 . #sec-3))
         (toc-tree (-reduce-from
                    (pcase-lambda (tree `(,title ,slug ,depth))
                      (push-in-last-seebling  depth `(,title . ,slug) tree))
                    nil
                    (-zip toc-titles toc-slugs toc-depths))))
    (when toc-tree
      (s-join "\n" (list
        "# Table of Contents"
        ""
        "<nav id=\"table-of-contents\">"
        "<ul>"
        (org-jekyll-render-toc toc-tree)
        "</ul>"
        "</nav>")))))

(defun org-jekyll-keyword (keyword contents info)
  "Transcode a KEYWORD element into Markdown format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let ((key (org-element-property :key keyword))
        (value (org-element-property :value keyword)))
    (cond
     ((string= key "TOC")
      (let ((depth (and (string-match "\\<[0-9]+\\>" value)
                        (string-to-number (match-string 0 value))))
            (localp (string-match-p "\\<local\\>" value)))
        (org-jekyll-toc depth info (and localp keyword))))
     (t
      (org-export-with-backend 'md keyword contents info)))))

(defun org-jekyll-template (contents info)
  "Return complete document string after Markdown conversion.
CONTENTS is the transcoded contents string.  INFO is a plist used
as a communication channel."
  (let* ((layout (plist-get info :jekyll-layout))
         (title (org-export-data (plist-get info :title) info))
         (date (org-export-data (org-export-get-date info "%Y-%m-%d") info))
         (author (and (plist-get info :with-author) (org-export-data (plist-get info :author) info)))
         (categories (plist-get info :jekyll-categories))
         ($preamble (s-join "\n" '(
              "---"
              "layout: ${layout}"
              "title: ${title}"
              "date: ${date}"
              "author: ${author}"
              "categories: ${categories}"
              "---"
              "")))
         (preamble (s-format $preamble 'aget `(("layout" . ,layout)
                                               ("title" . ,title)
                                               ("date" . ,date)
                                               ("author" . ,author)
                                               ("categories" . ,categories)))))
    (s-join "\n" (list preamble contents))))

(defun org-jekyll-paragraph (paragraph contents info)
  "Transcode PARAGRAPH element into Markdown format.
CONTENTS is the paragraph contents.  INFO is a plist used as
a communication channel."
  (cond
   ;; Standalone image (from ox-html)
   ((org-html-standalone-image-p paragraph info)
    (let ((caption
           (let ((raw (org-export-data
                       (org-export-get-caption paragraph) info))
                 (org-html-standalone-image-predicate
                  #'org-html--has-caption-p))
             (if (not (org-string-nw-p raw)) raw
               (concat "<span class=\"figure-number\">"
                       (format (org-html--translate "Figure %d:" info)
                               (org-export-get-ordinal
                                (org-element-map paragraph 'link
                                  #'identity info t)
                                info nil #'org-html-standalone-image-p))
                       " </span>"
                       raw))))
          (label (and (org-element-property :name paragraph)
                      (org-export-get-reference paragraph info)))
          ;; Update image source path to lookup for them into "assets/slug-title/img.svg""
          (new-contents (replace-regexp-in-string
                         "src=\".+?\""
                         (lambda (src-attr)
                           (let ((img-name (file-name-nondirectory
                                            (s-chop-prefix
                                             "src=\"" (s-chop-suffix "\"" src-attr))))
                                 (slug-title (slugify (org-export-data (plist-get info :title) info))))
                             (format "src='{{ \"assets/%s/%s\" | absolute_url }}'" slug-title img-name)))
                        contents)))
      (org-html--wrap-image new-contents info caption label)))
   ;; Fallback on ox-md
   (t (org-export-with-backend 'md paragraph contents info))))



;;; Interactive function

;;;###autoload
(defun org-jekyll-export-as-jekyll (&optional async subtreep visible-only)
  "Export current buffer to a Jekyll buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Export is done in a buffer named \"*Org JEKYLL Export*\", which will
be displayed when `org-export-show-temporary-export-buffer' is
non-nil."
  (interactive)
  (org-export-to-buffer 'jekyll "*Org JEKYLL Export*"
    async subtreep visible-only nil nil (lambda () (text-mode))))

;;;###autoload
(defun org-jekyll-convert-region-to-jekyll ()
  "Assume the current region has Org syntax, and convert it to Jekyll.
This can be used in any buffer.  For example, you can write an
itemized list in Org syntax in a Jekyll buffer and use
this command to convert it."
  (interactive)
  (org-export-replace-region-by 'jekyll))


;;;###autoload
(defun org-jekyll-export-to-jekyll (&optional async subtreep visible-only)
  "Export current buffer to a Jekyll file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return output file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".md" subtreep)))
    (org-export-to-file 'jekyll outfile async subtreep visible-only)))

;;;###autoload
(defun org-jekyll-publish-to-jekyll (plist filename pub-dir)
  "Publish an org file to Jekyll.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'jekyll filename ".md" plist pub-dir))


(provide 'ox-jekyll)
