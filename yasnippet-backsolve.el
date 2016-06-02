;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Regex Constants
(defconst yas--free-text-group-regex
  "\\(.*\\)" 
  "A regexp to match free text entry from a snippit. Does not
recognize new line")

(defconst yas--prefix-group-regex
    "\\([ \t]*\\)"
  "A regexp to match stuff at the start of the of each line of
  the template regex.")

(defconst yas--any-char-regex
    "\\([[:unibyte:]]*?\\)"
  "A regexp to match arbitrary unlimited characters but in a
  non-greedy way.")

(defconst yas--blank-line-regex
  "^[ \t]*$"
  "Regex to match a single blank line")

(defconst yas--whitespace-regex
  "[ \t]*"
  "Regex to match whitespace")
       
;; Functions 
(defun ljj--get-regex-groups (string regex) 
  "Extracts the match groups from a regex match on a string"
  (string-match regex string)
  (setq return ())
  (setq count 1)
  (while (setq match (match-string count string))
    (setq return (append  return (list match)))
    (setq count (1+ count)))
  return)

(defun yas--template-to-regex (templateStr)
  "Take a snippet template string and transform it into a regular
expression capable of searching"
  ;; First create a snippet from the template.
  (let ((snippet (yas--make-snippet))
        count
        (exit-flag 'nil)
        return
        curr-buffer)
    
    ;; Create buffer to do parsing in
    (setq temp-buffer (generate-new-buffer-name "temp-buffer"))
    (get-buffer-create temp-buffer)
    (setq curr-buffer (current-buffer))
    (set-buffer temp-buffer)
    (text-mode)                         ; Use an inoculous mode to parse the snippet in
    (insert templateStr)
    (goto-char (point-min))

    ;; Parse the template
    (yas--snippet-parse-create-no-indent snippet)

    ;; Sort and link each field
    (yas--snippet-sort-fields-by-start snippet)

    ;; Get the fields variable
    (setq fields (yas--snippet-fields snippet))

    ;; Replace regex chars with regular text (eg . -> \. and ] -> \])
    (yas--buffer-to-regex)    

    ;; Special consideration is needed for the exit location
    ;; regex. Here we check if the snippet specifies one at all:
    (if (not (yas--snippet-exit snippet))
        (setq exit-flag 't))
    
    ;; Loop on the fields
    (setq count 2)
    (dolist (field fields)
      ;; First check if we have passed by the $0 marker
      (if (not exit-flag)
          (if (> (yas--field-start field)
                 (yas--exit-marker (yas--snippet-exit snippet)))
              (progn
               ;; Insert a regex marger for $0
               (goto-char (yas--exit-marker (yas--snippet-exit snippet)))
               (insert yas--any-char-regex)
               ;; Add the count to the return object
               (setq count (+ count 1))
               ;; Indicate that we have marked the exit regex
               (setq exit-flag 't))))
      
      ;; Replace the text for each field with a matchgroup regex
      (delete-region (yas--field-start field)
                     (yas--field-end field))
      (goto-char (yas--field-start field))
      (insert yas--free-text-group-regex)
      
      ;; Replace the mirror fields with a mirror regex. eg \1, \2, etc.
      (dolist (mirror (yas--field-mirrors field))
        (delete-region (yas--mirror-start mirror)
                       (yas--mirror-end mirror))
        (goto-char (yas--mirror-start mirror))
        (insert "\\")
        (insert (number-to-string count)))
      (setq count (+ count 1)))

    ;; If we never passed the $0 marker, we handle it here.
    (if (not exit-flag)
        (progn
         ;; Insert a regex marger for $0
         (goto-char (yas--exit-marker (yas--snippet-exit snippet)))
         (insert yas--any-char-regex)
         ;; Add the count to the return object
         (setq count (+ count 1))
         ;; Indicate that we have marked the exit regex
         (setq exit-flag 't)))
    
    ;; insert line prefixes. First line prefix is a free text match
    ;; group. All other lines mirror this match. This means that
    ;; however much white space appears before the first line must
    ;; also appear before subsequent lines.
    (yas--buffer-add-line-prefixes yas--prefix-group-regex "\\1" nil)

    ;; Replace blank lines with white space regex
    (goto-char (point-min))
    (while (re-search-forward yas--blank-line-regex nil t)
      ;; replace the match
      (replace-match yas--whitespace-regex))
    
    ;; Get regex string to return
    (setq return (buffer-substring (point-min) (point-max)))

    ;; Switch back to current buffer
    (switch-to-buffer curr-buffer)
    
    ;; Kill the child buffer
    (kill-buffer temp-buffer)

    ;; Decrement yas-snippt-id-seed to hide the creation of the
    ;; snippet in the temp buffer. Otherwise, the id will count
    ;; upwards when it should not.
    (yas--snippet-previous-id)
    
    ;; Return the regex object
    return))

(defun yas--buffer-to-regex ()
  "This function transforms buffer into a regex that exactly
matches the buffer. This has the effect of changing all special
regex symbols to include a break character if needed. Thus
'[asdf]' would be transformed to '\[asdf]' and so on.

Because the buffer has markers in it which need to be saved, we
cannot simply replace the buffer string with a string generated
by regexp-quote. Instead, we create 'string' with
regexp-quote (which is always longer) and scan through the
buffers adding characters as needed."
  (save-excursion
    (let (string index) 
      (setq string (buffer-substring (point-min) (point-max)))
      (setq string (regexp-quote string)) ; Generate the regexp string
      (goto-char (point-min))             ; set position to 0
      (setq index 0)                      ; set index to 0

      ;; Scan through buffer and replace insert chars when there is a
      ;; discrepancy between the buffer and string.
      (while (< (point) (point-max)) 
        (if (equal (elt string index) (char-after (point)))
            (goto-char (+ (point) 1))
          (insert (elt string index)))
        (setq index (+ index 1))))))

(defun yas--line-is-blank ()
  "determine if line at point is empty or not. Empty here means
  line contains only whitespace chars."
  (save-excursion
    (let ((start-line (line-number-at-pos)))
      ;; If we match on the regex compare line numbers otherwise
      ;; return nil.
      (if (re-search-forward yas--blank-line-regex nil t)
          (= start-line (line-number-at-pos))
        nil))))

(defun yas--buffer-add-line-prefixes (firstLine laterLines markAll)
  "Add prefix regular expressions to the start of each line of
  the buffer. The first line of the buffer will be prefixed by
  The string \"firstLine\". All remaining lines will be prefixed
  by the string \"laterLines\". Blank lines are ignored if
  markAll is nil. The first line is always marked even if blank."

  (save-excursion
    (let (index max)

      ;; Insert the first line prefix
      (goto-char (point-min))
      (insert-before-markers firstLine)
      
      ;; Insert later lines prefixes
      (forward-line)
      (setq index 1)
      (setq max (count-lines (point-min) (point-max)))
      (while (< index max)
        (beginning-of-line)
        (if (or markAll (not (yas--line-is-blank)))
            (insert-before-markers laterLines))
        (forward-line)
        (setq index (+ 1 index))))))

(defun yas--fill-template (templateStr groups)
  "Take a template string and fill in fields with the strings in
  \"groups\"."
  ;; First create a snippet from the template.

  (let ((snippet (yas--make-snippet))
        count
        offset
        (exit-flag 'nil))
    
    ;; Create buffer to do parsing in
    (setq start (point))
    (insert templateStr)

    ;; Narow
    (save-restriction
      (narrow-to-region start (point))
      (goto-char (point-min))
      
      ;; Parse the template
      (yas--snippet-parse-create-no-indent snippet)
      
      ;; Create keymap overlay for snippet
      (setf (yas--snippet-control-overlay snippet)
            (yas--make-control-overlay snippet (point-min) (point-max)))

      ;; Insert first match group at beginning of each line
      (yas--buffer-add-line-prefixes (nth 0 groups) (nth 0 groups) t)
      
      ;; Sort "by start" order:
      (yas--snippet-sort-fields-by-start snippet)
      
      ;; Special consideration is needed for the exit location
      ;; regex. Here we check if the snippet specifies one at all:
      (if (not (yas--snippet-exit snippet))
          (setq exit-flag 't))
      
      ;; Insert correct values before sorting (skipping the first value
      ;; in groups)
      (setq count 0)
      (setq offset 1)
      (while (setq field (nth count (yas--snippet-fields snippet)))
        ;; First check if we have passed by the $0 marker
        (if (not exit-flag)
            (if (> (yas--field-start field) (yas--exit-marker (yas--snippet-exit snippet)))
                (progn
                  ;; Insert a regex marker for $0
                  (goto-char (yas--exit-marker (yas--snippet-exit snippet)))
                  (insert (nth (+ count offset) groups))
                  ;; Add the count to the return object
                  (setq offset (+ offset 1))
                  ;; Indicate that we have marked the exit regex
                  (setq exit-flag 't))))
        
        ;; Now enter group text into the current field
        (yas--move-to-field snippet field)
        (insert (nth (+ count offset) groups))
        (setq count (+ count offset)))

      ;; First check if we have passed by the $0 marker
      (if (not exit-flag)
          (progn
            ;; Insert a regex marker for $0
            (goto-char (yas--exit-marker (yas--snippet-exit snippet)))
            (insert (nth (+ count offset) groups))
            ;; Add the count to the return object
            (setq offset (+ offset 1))
            ;; Indicate that we have marked the exit regex
            (setq exit-flag 't)))
      
      ;; Sort fields
      (yas--snippet-sort-fields snippet)
      
      ;; Exit the snippet immediately if no fields
      (unless (yas--snippet-fields snippet)
        (yas-exit-snippet snippet))

      ;; Now, schedule a move to the first field
      (let ((first-field (car (yas--snippet-fields snippet))))
        (when first-field
          (yas--move-to-field snippet first-field))))))

(defun yas--snippet-sort-fields-by-start (snippet)
  "Sort the fields of SNIPPET in navigation order."
  (setf (yas--snippet-fields snippet)
        (sort (yas--snippet-fields snippet)
              #'yas--snippet-field-compare-by-start)))

(defun yas--snippet-field-compare-by-start (field1 field2)
  "Compare through the field's start point"
  (< (yas--field-start field1)
     (yas--field-start field2)))

(defun yas--match-and-revive (templateStr)
  "Function which takes a snippet template and 1. searches for a
  match in buffer 2. extracts the match groups 3. removes the
  match region and 4. replaces region with filled
  template. Returns t if the search for a match is successful and
  nil otherwise."

  (let ((regex (yas--template-to-regex templateStr)) (groups nil) (start) (end))
    (message regex)
    (if (re-search-forward regex nil t)
        (progn
          (setq start (match-beginning 0))
          (setq end (match-end 0))
          (setq groups
                (ljj--get-regex-groups
                 (buffer-substring start end)
                 regex))
          ;; Remove region starting at end of first match group. This leaves
          ;; any prefix white-space present to prevent indentation being
          ;; effected.
          (delete-region start end)
          (yas--fill-template templateStr groups)
          t)
      nil)))

(defun yas--get-templates-from-key (key)
  "Return a list of templates based on a key."
  (mapcar #'(lambda (temp)
              (cdr temp))
          (mapcan #'(lambda (table)
                      (yas--fetch table key))
                  (yas--get-snippet-tables))))

(defun yas--snippet-parse-create-no-indent (snippet)
  "Parse a recently inserted snippet template, creating all
necessary fields, mirrors and exit points.

Meant to be called in a narrowed buffer, does various passes

This is the same as yas--snippet-parse-create but skips the final
indent step. This is hack to prevent bad format changing
issues. Perhaps modify yas--snippet-parse-create in the future to
have an option to skip the indentation process."
  (let ((parse-start (point)))
    ;; Reset the yas--dollar-regions
    ;;
    (setq yas--dollar-regions nil)
    ;; protect just the backquotes
    ;;
    (yas--protect-escapes nil '(?`))
    ;; replace all backquoted expressions
    ;;
    (goto-char parse-start)
    (yas--save-backquotes)
    ;; protect escaped characters
    ;;
    (yas--protect-escapes)
    ;; Parse indent markers: `$>'.
    (goto-char parse-start)
    (yas--indent-parse-create snippet)
    ;; parse fields with {}
    ;;
    (goto-char parse-start)
    (yas--field-parse-create snippet)
    ;; parse simple mirrors and fields
    ;;
    (goto-char parse-start)
    (yas--simple-mirror-parse-create snippet)
    ;; parse mirror transforms
    ;;
    (goto-char parse-start)
    (yas--transform-mirror-parse-create snippet)
    ;; calculate adjacencies of fields and mirrors
    ;;
    (yas--calculate-adjacencies snippet)
    ;; Delete $-constructs
    ;;
    (save-restriction
      (widen)
      (yas--delete-regions yas--dollar-regions))
    ;; Make sure to do this insertion *after* deleting the dollar
    ;; regions, otherwise we invalidate the calculated positions of
    ;; all the fields following $0.
    (let ((exit (yas--snippet-exit snippet)))
      (goto-char (if exit (yas--exit-marker exit) (point-max))))
    (when (eq yas-wrap-around-region 'cua)
      (setq yas-wrap-around-region ?0))
    (cond ((and yas-wrap-around-region yas-selected-text)
           (insert yas-selected-text))
          ((and (characterp yas-wrap-around-region)
                (get-register yas-wrap-around-region))
           (insert (prog1 (get-register yas-wrap-around-region)
                     (set-register yas-wrap-around-region nil)))))
    ;; restore backquoted expression values
    ;;
    (yas--restore-backquotes)
    ;; restore escapes
    ;;
    (goto-char parse-start)
    (yas--restore-escapes)
    ;; update mirrors for the first time
    ;;
    (yas--update-mirrors snippet)
    ;; indent the best we can
    ;;
    (goto-char parse-start)
    ;; (yas--indent snippet)
    ))

(defun yas--snippet-previous-id ()
  "Function to decrement the yas--snippet-id-seed. This is used
  when you wish to create a snippet without the user being aware
  of it. The only place this is used so far is in
  yas--template-to-regex."
  (cl-incf yas--snippet-id-seed -1))

(defun yas--snippet-is-simple (templateStr)
  "Returns t if the snippet has no tranformation fields and no
  transformation mirrors. We refer to this type of snippet as
  simple."
  
  (let ((snippet (yas--make-snippet))
        return
        field
        mirror
        curr-buffer)

    ;; Default return value to t.
    (setq return 't)
    
    ;; Create buffer to do parsing in
    (setq temp-buffer (generate-new-buffer-name "temp-buffer"))
    (get-buffer-create temp-buffer)
    (setq curr-buffer (current-buffer))
    (set-buffer temp-buffer)
    (text-mode)                         ; Use an inoculous mode to parse the snippet in
    (insert templateStr)
    (goto-char (point-min))
    
    ;; Parse create the snippet
    (yas--snippet-parse-create-no-indent snippet)

    ;; Loop on fields
    (dolist (field (yas--snippet-fields snippet))
      ;; If field is not simple, nil the return value.
      (if (yas--apply-transform field field 't)
          (setq return 'nil))
      ;; Subloop on mirrors
      (dolist (mirror (yas--field-mirrors field))
        ;; If mirror is not simple, nil the return value.
        (if (yas--apply-transform mirror field 't)
            (setq return 'nil))))
    
    ;; Switch back to current buffer
    (switch-to-buffer curr-buffer)
    
    ;; Kill the child buffer
    (kill-buffer temp-buffer)

    ;; Return the result
    return))

(defun yas-backsolve ()
  "This is the interactive function to revive a snippet. \"key\"
  is the same key witch launches the snippet"
  (interactive)
  (let ((templatesByKey)
        (templateStr)
        (key (yas-completing-prompt "Key: " (yas-active-keys))))
    ;; Get all templates by key
    (setq templatesByKey (yas--get-templates-from-key key))
    ;; If zero are found do nothing and message an error (this should
    ;; never happen)
    (if (null templatesByKey) (message "Key not found!")
      ;; If one is found, match and revive
      (if (= 1 (length templatesByKey))         
          (setq templateStr (yas--template-content (nth 0 templatesByKey)))
        ;; If multiple are found, prompt to choose
        (setq templateStr
         (yas--template-content
          (yas--prompt-for-template templatesByKey "Choose a template: "))))
      ;; Check if the snippet is simple
      (if (yas--snippet-is-simple templateStr)
          (if (not (yas--match-and-revive templateStr))
              (message "Backsolve not found for given key!"))
        (message "This snippet format is not yet supported by yas-backsolve!")))))

(provide 'yasnippet-backsolve)
