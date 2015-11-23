;;; Copyright (c) 2015 Juhani Imberg
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

(require-extension srfi-1
                   srfi-13
                   posix
                   extras
                   ;; these must be installed
                   srfi-19
                   utf8)

;;; output formatting functions

(define (as-percentage val)
  (string-append
   (number->string (inexact->exact (round (* 100 val))))
   "%"))

(define vertical-bar-characters (string->list " ▏▎▍▌▋▊▉█"))
(define horizontal-bar-characters (string->list " ▁▂▃▄▅▆▇█"))

(define (make-bar-segment val characters)
  (let ((ind (min 8 (max 0 (inexact->exact (floor val))))))
    (list-ref characters ind)))

(define (make-bar val len characters)
  (if (zero? len)
      '()
      (cons (make-bar-segment val
                              characters)
            (make-bar (- val 8)
                      (- len 1)
                      characters))))

(define (as-bar val #!key (len 10) (characters vertical-bar-characters))
  (apply string (make-bar (* 8 val len) len characters)))

(define (as-time val #!key (format "~T"))
  (if val
      (format-date format (seconds->date val #f) #f)
      #f))

;;; i3 display related functions

(define i3-start "{\"version\":1}[[]")

(define (i3-make-segment parts #!key (color ""))
  (let ((parts (filter values (if (list? parts)
                                  parts
                                  (list parts)))))
    (if (not (null? parts))
        (string-append
         "{"
         (if (> (string-length color) 0)
             (string-append "\"color\":\"" color "\",")
             "")
         "\"full_text\":\""
         (string-join parts)
         "\"}"))))

(define (i3-make-status #!rest segments)
  (string-append
   ",["
   (string-join (filter values segments)
                ",")
   "]"))

;;; battery related functions

(define battery-path "/sys/class/power_supply/BAT0/")

(define (battery-exists?)
  (directory-exists? battery-path))

(define (get-battery-path name)
  (string-append battery-path name))

(define (get-battery-int name)
  (with-input-from-file (get-battery-path name)
    (lambda ()
      (string->number (read-line)))))

(define (get-battery-charge)
  (let ((charge-now (get-battery-int "charge_now"))
        (charge-full (get-battery-int "charge_full")))
    (/ charge-now charge-full)))

(define (get-battery-status)
  (with-input-from-file (get-battery-path "status")
    (lambda ()
      (string-trim (read-line)))))

(define (get-battery-time)
  (let* ((voltage (/ (get-battery-int "voltage_now")
                     1000))
         (current (get-battery-int "current_now"))
         (charge (get-battery-int "charge_now"))
         (charge-full (get-battery-int "charge_full"))
         (status (get-battery-status))
         (capacity (/ charge voltage))
         (capacity-full (/ charge-full voltage))
         (rate (/ current voltage)))
    (cond ((string= status "Discharging")
           (/ (* 3600 capacity)
              rate))
          ((string= status "Charging")
           (/ (* 3600 (- capacity-full capacity))
              rate))
          (else
           #f))))

;;; proc functions

(define (get-load-list)
  (with-input-from-file "/proc/loadavg"
    (lambda ()
      (map string->number (take (string-tokenize (read-line))
                                3)))))

(define (get-memory-int line)
  (string->number (list-ref (string-tokenize line)
                            1)))

(define (get-mem-used)
  (with-input-from-file "/proc/meminfo"
    (lambda ()
      (let* ((lines (read-lines))
             (mem-total-line (list-ref lines 0))
             (mem-available-line (list-ref lines 2))
             (mem-total (get-memory-int mem-total-line))
             (mem-available (get-memory-int mem-available-line)))
        (/ (- mem-total mem-available)
           mem-total)))))

(define last-cpu-stats '((0 0)))

(define (update-cpu-stats! total idle smooth-length)
  (set! last-cpu-stats
        (cons (list total idle)
              (if (> (length last-cpu-stats)
                     (- smooth-length 1))
                  (take last-cpu-stats (- smooth-length 1))
                  last-cpu-stats))))

(define (average-cpu-stats)
  (list (/ (apply + (map first last-cpu-stats)) (length last-cpu-stats))
        (/ (apply + (map second last-cpu-stats)) (length last-cpu-stats))))

(define (get-cpu #!key (smooth 1))
  (let* ((line (with-input-from-file "/proc/stat"
                 read-line))
         (vals (map string->number (drop (string-tokenize line)
                                         1)))
         (total (apply + vals))
         (idle (fourth vals))
         (average-last (average-cpu-stats))
         (diff-idle (- idle (second average-last)))
         (diff-total (- total (first average-last))))
    (begin
      (update-cpu-stats! total idle smooth)
      (/ (- diff-total diff-idle)
         diff-total))))

;;; what we want to get

(define (make-line)
  (i3-make-status
   (i3-make-segment
    (as-bar (get-cpu) len: 1 characters: horizontal-bar-characters)
    color: "#f92672")
   (i3-make-segment
    (as-bar (get-mem-used) len: 1 characters: horizontal-bar-characters)
    color: "#e6db74")
   (if (battery-exists?)
       (i3-make-segment
        (list
         (as-bar (get-battery-charge) len: 1 characters: horizontal-bar-characters)
         (get-battery-status)
         (as-time (get-battery-time)))
        color: "#66d9ef")
       #f)
   (i3-make-segment (format-date "~1 ~T" (current-date)))))

(define (loop-forever)
  (display (make-line))
  (flush-output)
  (sleep 1)
  (loop-forever))

(let ((arg (command-line-arguments)))
  (display i3-start)
  (loop-forever))
