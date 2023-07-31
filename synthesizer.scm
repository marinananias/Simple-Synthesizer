; CSC 151 (Spring 2023)
; Mini-project 7: A Simple Synthesizer
; Author: Marina Ananias
; Date: 2023-04-26
; Acknowledgements: Function synthesize-square-wave-note and its helpers, all before "Part 1," were adapted from the ASDR lab, done by myself and Cadence
;                   https://csc151.cs.grinnell.edu/mps/mp07.html

(import audio)

;;; (vec sample-rate duration) -> vector?
;;;     sample-rate: number?, a non-negative integer
;;;     duration: number?, a non-negative number
;;; Returns a vector with range 0 to the product of sampl-rate and duration.
(define vec 
    (lambda (sample-rate duration)
        (vector-range 0 (* sample-rate duration))))

;;; (samples-per-wave sample-rate frequency) -> number?, a non-negative integer
;;;     sample-rate: number?, a non-negative integer
;;;     frequency: number?, a non-negative number
;;; Returns the number of samples-per-wave
(define samples-per-wave
    (lambda (sample-rate frequency)
        (/ sample-rate frequency)))

;;; (square-helper sample-rate frequency v) -> number?
;;;     sample-rate: number?, a non-negative integer
;;;     frequency: number?, a non-negative number
;;;     v: number?
;;; Returns either -1 or 1 depending if the index value of the vector is before or after the middle value.
(define square-helper
    (lambda (sample-rate frequency v)
        (if (< (remainder v (samples-per-wave sample-rate frequency)) (/ (samples-per-wave sample-rate frequency) 2))
            -1
             1)))

;;; (square-sample sample-rate frequency duration) -> vector?
;;;     sample-rate: number?, a non-negative integer
;;;     frequency: number?, a non-negative number
;;;     duration: number?, a non-negative number
;;; Returns a vector with values 1 and -1 by calling square-helper.
(define square-sample
    (lambda (sample-rate frequency duration)
      (vector-map 
        (lambda (v) 
            (square-helper sample-rate frequency v)) 
        (vec sample-rate duration))))

;;; (apply-envelope clip envelope) -> vector?
;;;     clip: vector? of samples [-1.0, 1.0]
;;;     envelope: vector? of samples in the range [0, 1]
;;; Mutates clip so that the envelope is applied to the clip.
(define apply-envelope
    (lambda (clip envelope)
        (vector-map * clip envelope)))

;;; (vec2 n total-samples) -> vector?
;;;     n: interger?
;;;     total-samples: number?, a non-negative integer
;;; Returns a vector starting at n and ending at the total-samples value.
(define vec2
    (lambda (n total-samples)
        (vector-range n (+ total-samples 1))))

;;; (simple-envelope total-samples) -> vector?
;;;     total-samples: number?, a non-negative integer
;;; Returns a vector that starts at 1 and linearly decreases.
(define simple-envelope
    (lambda (total-samples)
        (vector-map (lambda (i) 
            (if (equal? i 1)
                i
                (- 1 (/ (- i 1) total-samples)))) 
            (vec2 1 total-samples))))

;;; (sine-wave sample-rate frequency duration) -> vector?
;;;     sample-rate: number?, a non-negative integer
;;;     frequency: number?, a non-negative number
;;;     duration: number?, a non-negative number
;;; Retruns a vector that corresponds to the amplitude value of a sine wave.
(define sine-wave
  (lambda (sample-rate frequency duration)
    (vector-map
        (lambda (n) 
            (sin (* n (/ (* 2 pi) frequency))))
        (vec sample-rate duration))))

; (sine-wave 16000 1 10)

;;; (synthesize-square-note waveform sample-rate frequency duration) -> vector?
;;;     sample-rate: number?, a non-negative integer
;;;     frequency: number?, a non-negative number
;;;     duration: number?, a non-negative number
;;; Returns a vector of samples representing a single note syntheiszed from
;;; the given parameters.
(define synthesize-square-wave-note
    (lambda (sample-rate frequency duration)
        (apply-envelope (square-sample sample-rate frequency duration) (simple-envelope (* sample-rate duration)))))

; (sample-node (synthesize-square-wave-note 1600 440 1))

"Part 1: Multi-voice Synthesis"
"============================="
;;; (generte-note waveform sample-rate frequency duration) -> audio-node?
;;;     sample-rate: number?, a non-negative integer
;;;     frequency: number?, a non-negative number
;;;     duration: number?, a non-negative number
;;; Returns an audio node formed by a vector produced by the combination of a square and a sine wave.
(define generate-note
    (lambda (sample-rate frequency duration)
        (sample-node (vector-map (lambda (i) (/ i 2)) (vector-map + (sine-wave sample-rate frequency duration) (synthesize-square-wave-note sample-rate frequency duration))))))

(generate-note 16000 440 1)

"Part 2: The ADSR Envelope"
"========================="

;;; (augmented-generate-note waveform sample-rate frequency duration) -> audio-node?
;;;     sample-rate: number?, a non-negative integer
;;;     frequency: number?, a non-negative number
;;;     duration: number?, a non-negative number
;;;     asdr-list: list?, containing three floating points in the range 0.0 to 1.0.
;;;  Returns an audio node formed by vectors combined to form an "adsr envelope.
(define augmented-generate-note
    (lambda (sample-rate frequency duration asdr-list)
    (let* ([attack-n (list-ref asdr-list 0)]
           [decay-n (list-ref asdr-list 1)]
           [sustain-n (list-ref asdr-list 2)]
           [release-n (- 1 attack-n decay-n sustain-n)]
           [sample-number-adsr (lambda (asdr) (* (/ sample-rate frequency) asdr))])
                (vector-append
                    (vector-map (lambda (s) (/ s (sample-number-adsr attack-n))) (vector-range 0 (+ (sample-number-adsr attack-n) 1)))
                    (list->vector (map (lambda (s) (+ (/ s (sample-number-adsr decay-n)) (/ (list-ref (reverse (vector->list (vector-range 0 (+ (sample-number-adsr decay-n) 1)))) s) (* 2 (sample-number-adsr decay-n))))) (reverse (vector->list (vector-range 0 (+ (sample-number-adsr decay-n) 1))))))
                    (vector-map (lambda (s) (+ (- s s) 0.5)) (vector-range 0 (+ (sample-number-adsr sustain-n) 1)))
                    (list->vector (reverse (vector->list (vector-map (lambda (s) (/ s (* (sample-number-adsr attack-n) 2))) (vector-range 0 (sample-number-adsr release-n))))))))))

(sample-node (augmented-generate-note 100 2 1 (list 0.1 0.1 0.7)))
(augmented-generate-note 16000 440 1 (list 0.1 0.3 0.2))