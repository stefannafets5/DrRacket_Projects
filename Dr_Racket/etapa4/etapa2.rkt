#lang racket
(require racket/match)
(provide (all-defined-out))

;; În această etapă abstractizăm operatorii tipului PH astfel
;; încât să putem deriva ușor operațiile pentru diverse variante
;; de PH, în funcție de relația de ordine pe care se bazează
;; proprietatea de heap.
;;  - funcție afectată direct: merge
;;  - funcții afectate indirect: funcțiile care apelează merge,
;;     care vor avea nevoie să primească tipul de merge ca parametru
;;
;; Apoi, folosim tipul PH pentru a prelucra filme, unde un film
;; este reprezentat ca o structură cu 5 câmpuri: nume, rating, gen, 
;; durată, altele.
;; În Racket, există un mod simplu de a defini și manipula structuri,
;; descris în fișierul "tutorial.rkt".
;;
;; Fluxul de lucru recomandat pentru etapa 2 este:
;; - Copiați din etapa 1 funcțiile care rămân neschimbate
;; - Abstractizați după relația de ordine:
;;  * definiți operatorul mai general merge-f care primește, în plus
;;    față de merge, un comparator după care trebuie ordonate elementele
;;  * derivați din acest operator variantele cerute de merge
;;  * modificați acele funcții din etapa 1 care apelează merge, astfel
;;    încât funcția merge să fie parametru al funcției, nu un identificator
;;    legat la o valoare externă
;; - Citiți tutorialul despre structuri în Racket (fișierul "tutorial.rkt")
;; - Implementați funcțiile care prelucrează filme 


; TODO 0 (0p)
; Copiați din etapa 1 implementările funcțiilor
; de la TODO 1.

(define empty-ph '())
(define (val->ph T) (list T))
(define (ph-empty? ph) (null? ph))
(define (ph-root ph)
  (if (null? ph) #f (car ph)))
(define (ph-subtrees ph)
  (if (null? ph) #f (cdr ph)))


; TODO 1 (15p)
; Definiți funcția merge-f în formă curry, 
; astfel încât ulterior să definiți point-free
; funcțiile merge-min, merge-max și 
; merge-max-rating, ca aplicații parțiale
; ale lui merge-f.
;  - definiție point-free = o definiție care 
;    nu explicitează argumentul funcției
;   * ex: (define f add1) este o definiție point-free
;   * ex: (define (f x) (add1 x)) sau, echivalent,
;     (define f (λ (x) (add1 x))) nu sunt point-free
; merge-f = merge cu criteriul de comparație comp
; in: pairing heaps ph1, ph2, comparator comp
;     (ordinea și gruparea parametrilor
;     trebuie decisă de voi)
; out: union(ph1, ph2) astfel:
;   - union(vid, orice) = orice
;   - altfel, PH-ul cu root "mai puțin comp" 
;     devine primul fiu al celuilalt
;     (la egalitate, ph2 devine fiul lui ph1)
(define (merge-f comp)
  (lambda (ph1 ph2)
    (cond
      ((null? ph1) ph2)
      ((null? ph2) ph1)
      ((comp (car ph1) (car ph2))
       (append (list (car ph1)) (cons ph2 (cdr ph1))))
      (else
       (append (list (car ph2)) (cons ph1 (cdr ph2)))))))

; merge-max : PH x PH -> PH
; in: pairing heaps ph1, ph2
; precondiții: ph1, ph2 sunt max-PH-uri
; out: max-PH rezultat din union(ph1, ph2)
; RESTRICȚII (5p):
;  - Definiția trebuie să fie point-free.
(define merge-max (merge-f >=))

; merge-min : PH x PH -> PH
; in: pairing heaps ph1, ph2
; precondiții: ph1, ph2 sunt min-PH-uri
; out: min-PH rezultat din union(ph1, ph2)
; RESTRICȚII (5p):
;  - Definiția trebuie să fie point-free.
(define merge-min (merge-f <=))

; merge-max-rating : PH x PH -> PH
; in: pairing heaps ph1, ph2
; precondiții: ph1, ph2 conțin perechi cu punct
; (nume . rating) și sunt max-PH-uri ordonate
; după rating
; out: max-PH rezultat din union(ph1, ph2)
; RESTRICȚII (5p):
;  - Definiția trebuie să fie point-free.
(define merge-max-rating (merge-f (lambda (x y) (>= (cdr x) (cdr y)))))


; TODO 2 (10p)
; Redefiniți următoarele funcții din etapa 1 care
; apelează (direct sau indirect) merge, astfel
; încât funcția merge să fie dată ca parametru
; (pe prima poziție, ca în apelurile din checker):
;  - ph-insert
;  - list->ph
;  - two-pass-merge-LR
;  - ph-del-root

(define (ph-insert merge elem ph)
  (merge ph (list elem)))

(define (list->ph merge lst)
  (if (null? lst)
      '()
      (ph-insert merge (car lst) (list->ph merge (cdr lst)))))

(define (merge-two-by-two merge phs acc)
  (cond
    [(null? phs) acc]
    [(null? (cdr phs)) (merge acc (car phs))]
    [else (merge-two-by-two merge (cddr phs) (merge acc (merge (car phs) (cadr phs))))]))

(define (two-pass-merge-LR merge phs)
   (merge-two-by-two merge phs '()))

(define (ph-del-root merge ph)
  (if (null? ph) #f (two-pass-merge-LR merge (cdr ph))))


;; PARTEA A DOUA (cea în care prelucrăm filme)

;; Definim un film (movie) ca pe o structură cu 5 câmpuri:   
;; nume, rating, gen, durată, altele.
(define-struct movie (name rating genre duration others) #:transparent)


; TODO 3 (10p)
; lst->movie : [Symbol, Number, Symbol, [Int], [Symbol]] -> Movie
; in: listă lst cu 5 valori, în această ordine:
;     - numele reprezentat ca simbol (ex: 'the-lives-of-others)
;     - ratingul reprezentat ca număr (ex: 8.4)
;     - genul reprezentat ca simbol (ex: 'drama)
;     - durata reprezentată ca listă de ore și minute (ex: '(2 17))
;     - altele reprezentate ca listă de simboluri (ex: '(german))
; out: obiect de tip movie instanțiat cu cele 5 valori
; RESTRICȚII (10p):
;  - Nu identificați elementele listei, ci folosiți o funcțională.
(define (lst->movie lst)
  (apply make-movie lst))


; TODO 4 (10p)
; mark-as-seen : Movie -> Movie
; in: film m
; out: m actualizat astfel încât symbolul 'seen este
;      adăugat la începutul câmpului (listei) others
(define (mark-as-seen m)
  (make-movie
   (movie-name m)
   (movie-rating m)
   (movie-genre m)
   (movie-duration m)
   (cons 'seen (movie-others m))))


; TODO 5 (10p)
; mark-as-seen-from-list : [Movie] x [Symbol] -> [Movie]
; in: listă de filme movies, listă de nume seen
; out: lista movies actualizată astfel încât filmele
;      cu numele în lista seen sunt marcate ca văzute
; RESTRICȚII (10p):
;  - Nu folosiți recursivitate explicită.
;  - Folosiți cel puțin o funcțională.
(define (mark-as-seen-from-list movies seen)
  (map (lambda (m)
         (if (not (empty? (filter (is-same-name (movie-name m)) seen)))
             (mark-as-seen m)
             m))
       movies))

(define (is-same-name movie-name)
  (lambda (name)
    (eq? movie-name name)))
 
; TODO 6 (10p)
; extract-seen : [Movie] -> [Symbol]
; in: listă de filme movies
; out: lista numelor filmelor văzute din lista movies
;      (văzut = lista others conține 'seen)
; RESTRICȚII (10p):
;  - Nu folosiți recursivitate explicită.
;  - Nu folosiți funcționale de tip fold.
;  - Folosiți cel puțin o funcțională.
(define (extract-seen movies)
  (map movie-name
       (filter (lambda (m)
                 (member 'seen (movie-others m)))
               movies)))


; TODO 7 (15p)
; rating-stats : [Movie] -> (Number, Number)
; in: listă de filme movies
; out: pereche (rating-mediu-seen . rating-mediu-unseen)
;  - rating-mediu-seen = media rating-urilor filmelor văzute
;  - analog pentru unseen și filmele nevăzute
; (dacă nu există filme de un anumit fel, media este 0)
; RESTRICȚII
;  - Nu folosiți recursivitate explicită.
;  - Folosiți cel puțin o funcțională.
;  - Nu parcurgeți filmele din listă (sau părți ale listei)
;    mai mult decât o dată.
(define (rating-stats movies)
  (define seen-movies (filter (lambda (m) (member 'seen (movie-others m))) movies))
  (define unseen-movies (filter (lambda (m) (not (member 'seen (movie-others m)))) movies))

  (define (average lst)
    (if (null? lst)
        0
        (/ (apply + (map movie-rating lst)) (length lst))))
  
  (cons (average seen-movies) (average unseen-movies)))


; TODO 8 (10p)
; extract-name-rating : [Movie] -> [(Symbol, Number)]
; in: listă de filme movies
; out: listă de perechi (nume . rating) 
;      (o pereche pentru fiecare film din movies)
; RESTRICȚII (10p):
;  - Nu folosiți recursivitate explicită.
;  - Folosiți cel puțin o funcțională.
(define (extract-name-rating movies)
  (map (lambda (m) (cons (movie-name m) (movie-rating m))) movies))


; TODO 9 (10p)
; make-rating-ph : [Movie] -> PH
; in: listă de filme movies
; out: max-PH care conține perechile (nume . rating)
;      corespunzătoare filmelor din movies
;      (cu ordonare după rating)
;  - se inserează ultima pereche în PH-ul vid
;  - ...
;  - se inserează prima pereche în PH-ul de până acum
(define (make-rating-ph movies)
  (list->ph merge-max-rating (map (lambda (m) (cons (movie-name m) (movie-rating m))) movies)))


; TODO 10 (10p)
; before? : T1 x T2 x List
;           (List este o listă eterogenă)
; in: valori oarecare a, b, listă oarecare List
; out: true, dacă a = b sau a apare înaintea lui b în List
;      false, altfel
; RESTRICȚII (10p):
;  - Nu folosiți recursivitate explicită.
;  - Identificați în Help Desk funcționala findf
;    și folosiți-o.
(define (before? a b L)
  (equal? a b))


; TODO 11 (10p)
; make-genre-ph : [Movie] x [Symbol] -> PH
; in: listă de filme movies, listă de genuri genres
; out: PH care conține filme, astfel încât genul
;      unui nod părinte să apară în lista genres
;      înaintea genului fiilor săi      
;      (conform definiției din funcția before?)
;  - se inserează ultimul film în PH-ul vid
;  - ...
;  - se inserează primul film în PH-ul de până acum
; observație: când se inserează un film de același
; gen cu root-ul curent, noul film devine fiul
; root-ului 
(define (make-genre-ph movies genres)
  'your-code-here)

