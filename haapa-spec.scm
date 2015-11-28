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

(require-extension missbehave)
(require-library haapa-lib)

(describe "haapa-lib"
  (describe "formatting"
    (describe "percent"
      (it "must format 0 properly"
        (expect (as-percentage 0) (be "0%")))
      (it "must format 1 properly"
        (expect (as-percentage 1) (be "100%")))
      (it "must format inexact properly"
        (expect (as-percentage 17/31) (be "55%"))))
    (describe "bar"
      (it "must be of wanted length"
        (expect (string-length (as-bar 0 len: 33)) (be 33)))
      (it "must be able to use horizontal bar characters"
        (expect (as-bar 0.5 len: 1 characters: horizontal-bar-characters) (be "▄")))
      (it "must format inexact properly"
        (expect (as-bar 7/31) (be "██▎       "))))
    (describe "time"
      (it "must work with small time"
        (expect (as-time 40) (be "00:00:40")))
      (it "must work with large time"
        (expect (as-time (+ 30 (* 3600 4.5))) (be "04:30:30")))))
  (describe "i3"
    (describe "segment"
      (it "must be able to handle a single string"
        (expect (i3-make-segment "a")
                (be "{\"full_text\":\"a\"}")))
      (it "must be able to handle a list of strings"
        (expect (i3-make-segment '("a" "b" "c"))
                (be "{\"full_text\":\"a b c\"}")))
      (it "must be able to specify a color by hex"
        (expect (i3-make-segment '("a") color: "#aaaaaa")
                (be "{\"color\":\"#aaaaaa\",\"full_text\":\"a\"}")))
      (it "must be able to specify a color by name"
        (expect (i3-make-segment '("a") color: 'white)
                (be "{\"color\":\"#f8f8f2\",\"full_text\":\"a\"}"))))))
