#lang racket
(require racket/match)
(require "etapa2.rkt")
(provide (all-defined-out))

;; Această etapă continuă seria aplicațiilor heap-urilor  
;; de împerechere, pe care le vom folosi pentru a calcula
;; în mod dinamic mediana recenziilor unui film, simulând
;; condițiile din realitate - în care apar în permanență
;; noi recenzii pentru diverse filme.
;;    
;; Pentru a modela această dinamică folosim un flux de
;; perechi (nume-film . rating), pe baza căruia calculăm
;; un flux de stadii evolutive astfel:
;;  - fiecare stadiu este reprezentat ca listă de perechi
;;    * o pereche pentru fiecare film cu minim o recenzie
;;    * fiecare pereche este de forma
;;      (nume-film . mediană-rating-uri-primite-până-acum)
;;  - fiecare nouă recenzie determină actualizarea unei
;;    mediane, adică trecerea într-un alt stadiu,
;;    generând un nou element în fluxul rezultat
;;
;; Algoritmul folosit este următorul:
;;  Fluxul de perechi este transformat într-un flux de
;;  liste de cvartete (nume-film delta max-ph min-ph)
;;   - fiecare element din flux conține câte un cvartet 
;;     pentru fiecare film care are minim o recenzie
;;   - dacă filmul are un număr par de recenzii:
;;     - max-ph și min-ph au aceeași dimensiune
;;     - delta = size(max-ph) - size(min-ph) = 0
;;     - max-ph = max-PH cu cele mai mici rating-uri
;;     - min-ph = min-PH cu cele mai mari rating-uri
;;     - mediana este media rădăcinilor celor 2 PH-uri
;;   - dacă filmul are un număr impar de recenzii:
;;     - max-ph are un element în plus față de min-ph   
;;     - delta = size(max-ph) - size(min-ph) = 1
;;     - max-ph = max-PH cu cele mai mici rating-uri
;;     - min-ph = min-PH cu cele mai mari rating-uri
;;     - mediana este rădăcina lui max-ph
;;
;; Pentru completarea cu succes a etapei este necesar să
;; calculați medianele cu algoritmul descris în enunț.
;; În caz contrar, punctajul acordat de checker va fi retras.


; TODO 1 (45p)
; add-rating : (Symbol, Int, PH, PH) x Number
;              -> (Symbol, Int, PH, PH)
; in: cvartet (nume delta max-ph min-ph),
;     rating de adăugat
; out: cvartet actualizat prin adăugarea 
;      rating-ului, astfel:
;  - dacă rating <= root(max-ph)
;    inserează rating în max-ph, actualizând delta
;  - altfel
;    inserează rating în min-ph, actualizând delta
;  - dacă delta > 1
;    mută root(max-ph) în min-ph
;  - dacă delta < 0
;    mută root(min-ph) în max-ph
(define (insert-max rating max-ph) 
  (ph-insert merge-max rating max-ph))

(define (insert-min rating min-ph) 
  (ph-insert merge-min rating min-ph))

(define (move-to-min max-ph min-ph)
  (let ((moved-root (ph-root max-ph))
        (new-max-ph (ph-del-root merge-max max-ph))) ; max-ph fara radacina
    (list new-max-ph (insert-min moved-root min-ph)))) ; returnez lista

(define (move-to-max min-ph max-ph)
  (let ((moved-root (ph-root min-ph))
        (new-min-ph (ph-del-root merge-min min-ph))) ; min-ph fara radacina
    (list (insert-max moved-root max-ph) new-min-ph))) ; returnez lista

(define (add-rating quad rating)
  (let ((name (car quad))
        (delta (cadr quad))
        (max-ph (caddr quad))
        (min-ph (cadddr quad)))
     (if (or (ph-empty? max-ph) (<= rating (ph-root max-ph)))
         (let ((new-max (insert-max rating max-ph)) ; bag rating in max-ph
               (new-delta (+ delta 1)))
           ; delta trebuie <= 1
           (if (> new-delta 1)
               (let ((result (move-to-min new-max min-ph)))
                (let ((new-max2 (car result))
                      (new-min (car (cdr result))))
                  (list name 0 new-max2 new-min))) ; returnez lista in functie de delta
               (list name new-delta new-max min-ph)))
         ; bag rating in min-ph
         (let ((new-min (insert-min rating min-ph))
               (new-delta (- delta 1)))
           ; delta trebuie >= 0
           (if (< new-delta 0)
               (let ((result (move-to-max new-min max-ph)))
                 (let ((new-max (car result))
                      (new-min2 (car (cdr result))))
                   (list name 1 new-max new-min2))) ; returnez lista in functie de delta
               (list name new-delta max-ph new-min))))))

; TODO 2 (45p)
; reviews->quads : Stream<(Symbol, Number)> ->
;                  Stream<[(Symbol, Int, PH, PH)]>
; in: stream de perechi (nume . rating)
; out: stream de liste de cvartete
;      (nume delta max-ph min-ph)
;  - elementul k din rezultat corespunde primelor
;    k recenzii din input (ex: dacă primele 10
;    recenzii sunt pentru 3 filme distincte, al
;    10-lea element din fluxul rezultat conține o
;    listă de 3 cvartete - unul pentru fiecare film)
; RESTRICȚII (20p):
;  - Lucrați cu operatorii pe fluxuri, fără a
;    converti liste în fluxuri sau fluxuri în liste.

(define (add_rating_to_list last-list name rating)
  (map (lambda (quad) ; adaug rating daca e filmul in lista
         (if (equal? (car quad) name)
             (add-rating quad rating)
             quad)) last-list))

(define (is_inside last-list name) ; verific daca filmul e in lista
  (if (null? last-list) #f
      (if (equal? (car (car last-list)) name) #t
          (is_inside (cdr last-list) name))))

(define (reviews->quads_aux reviews last-list)
  (if (stream-empty? reviews)
      empty-stream
      (let* ((name (car (stream-first reviews)))
             (rating (cdr (stream-first reviews)))
             (new-list (if (is_inside last-list name) ; adaug rating sau creez si adaug cvartet
                               (add_rating_to_list last-list name rating)
                               (cons (add-rating (list name 0 '() '()) rating) last-list))))
        (stream-cons new-list (reviews->quads_aux (stream-rest reviews) new-list)))))

(define (reviews->quads reviews)
  (reviews->quads_aux reviews '()))

; TODO 3 (30p)
; quads->medians : Stream<[(Symbol, Int, PH, PH)]> ->
;                  Stream<[(Symbol, Number)]>  
; in: stream de liste de cvartete (ca mai sus)
; out: stream de liste de perechi (nume-film . mediană)
;  - mediana se calculează pe baza PH-urilor din
;    fiecare cvartet, conform algoritmului de mai sus
; RESTRICȚII (20p):
;  - Nu folosiți recursivitate explicită. Folosiți cel
;    puțin o funcțională pe fluxuri.
(define (get_median quad)
  (let ((name (car quad))
        (delta (cadr quad))
        (max-ph (caddr quad))
        (min-ph (cadddr quad)))
    (if (= delta 0)
        (cons name (/ (+ (ph-root max-ph) (ph-root min-ph)) 2))
        (cons name (ph-root max-ph)))))

(define (quads->medians quads)
  (if (stream-empty? quads)
      empty-stream
      (stream-cons (map get_median (stream-first quads))
                   (quads->medians (stream-rest quads)))))
