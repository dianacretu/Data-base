#lang racket
(define NULL 'null)

;====================================
;=            Cerința 1             =
;= Definirea elementelor de control =
;=          20 de puncte            =
;====================================

;= Funcțiile de acces

;Initializeaza o lista vida
(define init-database
  (λ ()
    '()))

;Creeaza tabelul cu numele coloanelor
(define create-table
  (λ (table columns-name)
    (list table columns-name)))

;Numele tabelului este primul element din tabel
(define get-name
  (λ (table)
    (car table)))

;Numele coloanelor sunt al doilea element din lista
(define get-columns
  (λ (table)
    (car (cdr table))))

;Toate tabelele compun baza de date
(define get-tables
  (λ (db)
    db))

;Cream o lista in care este doar tabelul care are denumirea
;egala cu table-name si extragem primul element
(define get-table
  (λ (db table-name)
    (car (filter (λ(x) (equal? (car x) table-name)) db))))

;Adaugam la lista baza de date, lista compusa din noul tabel
(define add-table
  (λ (db table)
    (append db (list table))))

;Cream o lista in care salvam doar elemetele care nu sunt
;egale cu table-name
(define remove-table
  (λ (db table-name)
    (filter (λ(x) (not (equal? (car x) table-name))) db)))

;= Pentru testare, va trebui să definiți o bază de date (având identificatorul db) cu următoarele tabele

;============================================================================================
;=                         Tabela Studenți                                                   =
;= +----------------+-----------+---------+-------+-------+                                  =
;= | Număr matricol |   Nume    | Prenume | Grupă | Medie |                                  =
;= +----------------+-----------+---------+-------+-------+                                  =
;= |            123 | Ionescu   | Gigel   | 321CA |  9.82 |                                  =
;= |            124 | Popescu   | Maria   | 321CB |  9.91 |                                  =
;= |            125 | Popa      | Ionel   | 321CC |  9.99 |                                  =
;= |            126 | Georgescu | Ioana   | 321CD |  9.87 |                                  =
;= +----------------+-----------+---------+-------+-------+                                  =
;=                                                                                           =
;=                                         Tabela Cursuri                                    =
;= +------+----------+-----------------------------------+---------------+------------+      =
;= | Anul | Semestru |            Disciplină             | Număr credite | Număr teme |      =
;= +------+----------+-----------------------------------+---------------+------------+      =
;= | I    | I        | Programarea calculatoarelor       |             5 |          2 |      =
;= | II   | II       | Paradigme de programare           |             6 |          3 |      =
;= | III  | I        | Algoritmi paraleli și distribuiți |             5 |          3 |      =
;= | IV   | I        | Inteligență artificială           |             6 |          3 |      =
;= | I    | II       | Structuri de date                 |             5 |          3 |      =
;= | III  | II       | Baze de date                      |             5 |          0 |      =
;= +------+----------+-----------------------------------+---------------+------------+      =
;============================================================================================
(define db '(("Studenți" ("Număr matricol" "Nume" "Prenume" "Grupă" "Medie")
                         (123 "Ionescu" "Gigel" "321CA" 9.82)
                         (124 "Popescu" "Maria" "321CB" 9.91)
                         (125 "Popa" "Ionel" "321CC" 9.99)
                         (126 "Georgescu" "Ioana" "321CD" 9.87))
             ("Cursuri" ("Anul" "Semestru" "Disciplină" "Număr credite" "Număr teme")
                        ("I" "I" "Programarea calculatoarelor" 5 2)
                        ("II" "II" "Paradigme de programare" 6 3)
                        ("III" "I" "Algoritmi paraleli și distribuiți" 5 3)
                        ("IV" "I" "Inteligență artificială" 6 3)
                        ("I" "II" "Structuri de date" 5 3)
                        ("III" "II" "Baze de date" 5 0))))



;====================================
;=            Cerința 2             =
;=         Operația insert          =
;=            10 puncte             =
;====================================

;Creeaza randul ce urmeaza sa fie inserat in tabel
(define (create-row columns record)
  (list
   ;Parcurgem fiecare nume de coloana
   (foldl
    (λ (col acc)
      ;Atunci cand gasim in record perechea care incepe
      ;cu numele coloanei, o adaugam la lista
      (if (assoc col record)
          (append acc (list (cdr (assoc col record))))
          (append acc (list NULL))))
    '()
    columns)))
        
;Atunci cand gasim elementul din baza de date egal cu table-name,
;adaugam la finalul listei respectiva randul creat
(define insert
  (λ (db table-name record)
    (map
     (λ (elem)
       (if (equal? (get-name elem) table-name)
           (append elem (create-row (get-columns (get-table db table-name)) record))
           elem))
     db)))



;====================================
;=            Cerința 3 a)          =
;=     Operația simple-select       =
;=             10 puncte            =
;====================================
;Parcurge fiecare rand al tabelului
;Aflam pe ce pozitie se afla coloana in lista cu numele coloanelor
;Adaugam la lista creata toate elementele de pe pozitia cu indicele aflat mai sus
(define (column-list col table)
  (list
   (foldl
    (λ(x acc)
      (append acc (list (list-ref x (index-of (get-columns table) col)))))
    '()
    (cddr table))))

(define simple-select
  (λ (db table-name columns)
    (let ((table (get-table db table-name)))
      (if (null? (cddr table))
          '()
          ;Parcurgem coloanele date si adaugam la lista noua fiecare lista a coloanei respective
          (foldl (λ (col-name acc)
                   (append acc (column-list col-name table))) '() columns)))))

;====================================
;=            Cerința 3 b)          =
;=           Operația select        =
;=            30 de puncte          =
;====================================

;Adaugam #t la lista daca randul respecta conditia si #f daca nu
(define (all-conditions x table conditions)
  (if (null? conditions)
      (list #t)
      (foldl (λ(cond acc)
               (if ((car cond) (list-ref x (index-of (get-columns table) (cadr cond))) (caddr cond))
                   (append acc (list #t))
                   (append acc (list #f))))
             '()
             conditions)))

;Parcurgem fiecare rand din tabel
;Adaugam in lista elementele care se afla in rand pe pozitia coloanei
(define (columns-list db col table conditions)
  (foldl
   (λ (x acc)
     ;Verificam daca respecta conditiile date in lista conditions
     (if (not (member #f (all-conditions x table conditions)))
         ;Daca respecta elementul de pe pozitia pe care se afla si numele coloanei
         ;este adaugat in lista
         (append acc (list (list-ref x (index-of (get-columns table) col))))
         acc))
   '()
   (cddr table)))


(define (find-min L)
  (list (apply min L)))

(define (find-max L)
  (list (apply max L)))

(define (find-count L)
  (list (length (remove-duplicates L))))

(define (find-sum L)
  (list (apply + L)))

(define (find-avg L)
  (list (/ (apply + L) (length L))))

(define (find-sort-asc L)
  (list (sort L <)))

(define (find-sort-desc L)
  (list (sort L >)))

;Trimit ca argumente perechea cu operatia si coloana pe care se realizeaza operatia respectiva
(define (inregistrari db columns table conditions)
  (case (car columns)
    ['min (find-min (columns-list db (cdr columns) table conditions))]
    ['max (find-max (columns-list db (cdr columns) table conditions))]
    ['count (find-count (columns-list db (cdr columns) table conditions))]
    ['sum (find-sum (columns-list db (cdr columns) table conditions))]
    ['avg (find-avg (columns-list db (cdr columns) table conditions))]
    ['sort-asc (find-sort-asc (columns-list db (cdr columns) table conditions))]
    ['sort-desc (find-sort-desc (columns-list db (cdr columns) table conditions))]
    ))

;Parcurgem lista de coloane
(define select
  (λ (db table-name columns conditions)
    (let ((table (get-table db table-name)))
      (foldl (λ (col-name acc)
               (if (pair? col-name)
                   (append acc (inregistrari db col-name table conditions))
                   (append acc (list (columns-list db col-name table conditions)))))
             '()
             columns)))) ;("Nume", (min, "Prenume"))

;====================================
;=             Cerința 4            =
;=           Operația update        =
;=            20 de puncte          =
;====================================

(define (chg elem values table)
  ;Pentru fiecare coloana din tabel
  (foldl
   (λ(y acc)
     ;Daca numele coloanei se afla in values, adaugam la lista creata valoarea din values
     ;Daca nu, pastram valoarea din tabel
     (if (assoc y values)
         (append acc (list (cdr (assoc y values))))
         (append acc (list (list-ref elem (index-of (get-columns table) y))))))
   '()    
   (get-columns table)))
         
;Parcurgem randurile tabelului
(define (change table values conditions)
  ;Pastram numele tabelului si numele coloanelor
  (append
   (list (get-name table)
         (get-columns table))
   (map
    (λ(elem)
          ;In cazul in care se respecta conditiile, se updateaza valorile
          (if (not (member #f (all-conditions elem table conditions)))
              (chg elem values table)
              elem)) 
        (cddr table))))

(define update
  (λ (db table-name values conditions)
    (let ((table (get-table db table-name)))
      ;Parcurgem baza de date
      ;In momentul in care am gasit tabelul
      ;Ii schimbam valorile indicate
      (map
       (λ(elem)
         (if (equal? elem table)
             (change table values conditions)
             elem))
       db))))

;====================================
;=             Cerința 5            =
;=           Operația remove        =
;=              10 puncte           =
;====================================


(define delete
  (λ (db table-name conditions)
    (let ((table (get-table db table-name)))
      ;Parcurgem baza de date
      (map
       (λ(elem)
         ;Daca gasim un tabel egal cu cel pe care il cautam
         (if (equal? elem table)
             ;Schimbam tabelul pastrand numele tabelului si numele coloanelor
             (append (list table-name
                           (get-columns table))
                     ;Pentru randuri cream o lista noua
                     (foldl (λ (x acc)
                              ;Daca respecta conditiile
                              (if (not (member #f (all-conditions x table conditions)))
                                  ;Nu este adaugat la lista
                                  acc
                                  (append acc (list x))))
                            '()
                            (cddr table)))  
             elem))
       db))))

;====================================
;=               Bonus              =
;=            Natural Join          =
;=            20 de puncte          =
;====================================
(define natural-join
  (λ (db tables columns conditions)
    'your-code-here))


;--------------CHECKER------------------------------

(define default-results '(#f 0 () your-code-here)) ; ce rezultate default sunt întoarse în exerciții
(define show-defaults 200) ; câte exerciții la care s-au întors rezultate default să fie arătate detaliat
(define prepend #t) (define name-ex '(testul testele trecut)) 
(define : 'separator) (define punct 'string) (define puncte 'string) (define BONUS 'string) (define total 0) (define all '()) (define n-ex 0) (define p-ex 0) (define defaults '())
(define (ex n sep p . s) (set! n-ex n) (set! p-ex p) (set! all (cons (list n p) all))) (define exercițiul ex) (define (p L) (map (λ (e) (display e) (display " ")) L) (newline))
(define (check-exp given expected) (check-exp-part "" 1 given expected)) (define (check-exp-part part per given expected) (check-test part per equal? given expected "diferă de cel așteptat"))
(define (check-in  given expected) (check-in-part  "" 1 given expected)) (define (check-in-part part per given expected) (check-test part per member given expected "nu se află printre variantele așteptate"))
(define (check-set given expected) (check-set-part  "" 1 given expected)) (define (check-set-part part per given expected) (check-test part per (λ (x y) (apply equal? (map list->seteqv `(,given ,expected)))) given expected "nu este echivalent cu cel așteptat"))
(define (check-set-unique given expected) (check-set-unique-part  "" 1 given expected)) (define (check-set-unique-part part per given expected) (check-test part per (λ (x y) (and (apply = (map length `(,given ,expected))) (apply equal? (map list->seteqv `(,given ,expected))))) given expected "nu este echivalent cu cel așteptat"))
(define (check-test part percent tester given expected err-m) (if (not (tester given expected)) (and (when (member given default-results) (set! defaults (cons (if (< percent 1) (cons n-ex part) n-ex) defaults)))
                                                                                                     (when (or (not (member given default-results)) (<= (length defaults) show-defaults))
                                                                                                       (p `(NU: la ,(car name-ex) ,(if (< percent 1) (cons n-ex part) n-ex) rezultatul ,given ,err-m : ,expected))))
                                                                  (let ((pts (* p-ex percent))) (and (if prepend (printf "+~v: " pts) (printf "OK: "))
                                                                                                     (p (list (car name-ex) (if (< percent 1) (cons n-ex part) n-ex) (caddr name-ex) '+ pts (if (= pts 1) 'punct 'puncte))) (set! total (+ total pts))))))
(define (sumar) (when (and (not (null? defaults)) (< show-defaults (length defaults))) (p `(... rezultatul implicit dat la ,(cadr name-ex) ,(reverse defaults)))) (p `(total: ,total puncte)))
(define Task ex) (define Bonus ex)

(Task 0 : 20 puncte)
(check-exp-part 'init-database .05 (get-tables (init-database)) '())
(check-exp-part 'create-table1 .05 (get-name (create-table "Company" '("Company_ID" "Company_Name" "Company_City"))) "Company")
(check-exp-part 'create-table2 .05 (get-columns (create-table "Company" '("Company_ID" "Company_Name" "Company_City"))) '("Company_ID" "Company_Name" "Company_City"))
(check-exp-part 'add-table1 .06 (map get-name (get-tables (add-table (init-database) (create-table "Test" '("Coloana1" "Coloana2" "Coloana3"))))) '("Test"))
(check-exp-part 'add-table2 .06 (sort (map get-name (get-tables (add-table db (create-table "Test" '("Coloana1" "Coloana2" "Coloana3"))))) string<?) '("Cursuri" "Studenți" "Test"))
(check-exp-part 'add-table3 .06 (sort (map get-name (get-tables (add-table (add-table db (create-table "Test" '("Coloana1" "Coloana2" "Coloana3"))) (create-table "Tabela1" '("Coloana1"))))) string<?) '("Cursuri" "Studenți" "Tabela1" "Test"))
(check-exp-part 'get-table1 .05 (get-columns (get-table db "Studenți")) '("Număr matricol" "Nume" "Prenume" "Grupă" "Medie"))
(check-exp-part 'get-table2 .05 (get-columns (get-table db "Cursuri")) '("Anul" "Semestru" "Disciplină" "Număr credite" "Număr teme"))
(check-exp-part 'get-tables .05 (sort (map get-name (get-tables db)) string<?) '("Cursuri" "Studenți"))
(check-exp-part 'remove-table1 .06 (map get-name (get-tables (remove-table db "Studenți"))) '("Cursuri"))
(check-exp-part 'remove-table2 .06 (map get-name (get-tables (remove-table db "Cursuri"))) '("Studenți"))
(check-exp-part 'remove-table3 .1 (map get-name (get-tables (remove-table (remove-table db "Cursuri") "Studenți"))) '())
(check-exp-part 'remove-table4 .1 (map get-name (get-tables (add-table (remove-table (remove-table db "Cursuri") "Studenți") (create-table "Test" '("Coloana1"))))) '("Test"))
(check-exp-part 'remove-table5 .1 (map get-name (get-tables (remove-table (add-table (remove-table (remove-table db "Cursuri") "Studenți") (create-table "Test" '("Coloana1"))) "Test"))) '())
(check-exp-part 'remove-table6 .1 (get-columns (get-table (remove-table db "Cursuri") "Studenți")) '("Număr matricol" "Nume" "Prenume" "Grupă" "Medie"))

(Task 1 : 20 puncte)
(check-exp-part 'simple-select1 0.05 (simple-select db "Studenți" '("Nume" "Prenume")) '(("Ionescu" "Popescu" "Popa" "Georgescu") ("Gigel" "Maria" "Ionel" "Ioana")))
(check-exp-part 'simple-select2 0.05 (simple-select db "Studenți" '("Număr matricol" "Medie")) '((123 124 125 126) (9.82 9.91 9.99 9.87)))
(check-exp-part 'simple-select3 0.05 (simple-select db "Studenți" '("Prenume" "Nume" "Număr matricol" "Medie")) '(("Gigel" "Maria" "Ionel" "Ioana") ("Ionescu" "Popescu" "Popa" "Georgescu") (123 124 125 126) (9.82 9.91 9.99 9.87)))
(check-exp-part 'simple-select4 0.05 (simple-select db "Cursuri" '("Număr credite" "Număr teme")) '((5 6 5 6 5 5) (2 3 3 3 3 0)))
(check-exp-part 'simple-select5 0.05 (simple-select db "Cursuri" '("Anul" "Semestru" "Număr teme")) '(("I" "II" "III" "IV" "I" "III") ("I" "II" "I" "I" "II" "II") (2 3 3 3 3 0)))
(check-exp-part 'simple-select6 0.05 (simple-select db "Cursuri" '("Disciplină" "Număr credite")) '(("Programarea calculatoarelor" "Paradigme de programare" "Algoritmi paraleli și distribuiți" "Inteligență artificială" "Structuri de date" "Baze de date") (5 6 5 6 5 5)))
(check-exp-part 'simple-select-&-insert1 0.1 (simple-select (insert db "Cursuri" '(("Disciplină" . "Matematică"))) "Cursuri" '("Disciplină" "Număr credite")) '(("Programarea calculatoarelor" "Paradigme de programare" "Algoritmi paraleli și distribuiți" "Inteligență artificială" "Structuri de date" "Baze de date" "Matematică") (5 6 5 6 5 5 null)))
(check-exp-part 'simple-select-&-insert2 0.1 (simple-select (insert db "Cursuri" '(("Disciplină" . "Matematică") ("Anul" . "I"))) "Cursuri" '("Disciplină" "Număr credite" "Anul" "Număr credite")) '(("Programarea calculatoarelor" "Paradigme de programare" "Algoritmi paraleli și distribuiți" "Inteligență artificială" "Structuri de date" "Baze de date" "Matematică") (5 6 5 6 5 5 null) ("I" "II" "III" "IV" "I" "III" "I") (5 6 5 6 5 5 null)))
(check-exp-part 'simple-select-&-insert3 0.1 (simple-select (insert (insert db "Cursuri" '(("Disciplină" . "Matematică") ("Anul" . "I"))) "Cursuri" '(("Disciplină" . "Limbaje formale și automate") ("Anul" . "III")  ("Număr teme" . 1) )) "Cursuri" '("Disciplină" "Număr credite" "Anul" "Număr teme")) '(("Programarea calculatoarelor" "Paradigme de programare" "Algoritmi paraleli și distribuiți" "Inteligență artificială" "Structuri de date" "Baze de date" "Matematică" "Limbaje formale și automate") (5 6 5 6 5 5 null null) ("I" "II" "III" "IV" "I" "III" "I" "III") (2 3 3 3 3 0 null 1)))
(check-exp-part 'simple-select-&-insert4 0.1 (simple-select (insert (insert (insert db "Cursuri" '(("Disciplină" . "Matematică") ("Anul" . "I"))) "Cursuri" '(("Disciplină" . "Limbaje formale și automate") ("Anul" . "III")  ("Număr teme" . 1) )) "Studenți" '(("Număr matricol" . 134) ("Nume" . "Dumitru") ("Prenume" . "Gigel"))) "Studenți" '("Prenume" "Nume" "Număr matricol")) '(("Gigel" "Maria" "Ionel" "Ioana" "Gigel") ("Ionescu" "Popescu" "Popa" "Georgescu" "Dumitru") (123 124 125 126 134)))
(check-exp-part 'simple-select-&-insert5 0.1 (simple-select (insert (insert (insert db "Cursuri" '(("Disciplină" . "Matematică") ("Anul" . "I"))) "Cursuri" '(("Disciplină" . "Limbaje formale și automate") ("Anul" . "III")  ("Număr teme" . 1) )) "Studenți" '(("Număr matricol" . 134) ("Nume" . "Dumitru") ("Prenume" . "Gigel"))) "Studenți" '("Prenume" "Nume" "Medie")) '(("Gigel" "Maria" "Ionel" "Ioana" "Gigel") ("Ionescu" "Popescu" "Popa" "Georgescu" "Dumitru") (9.82 9.91 9.99 9.87 null)))  
(check-exp-part 'simple-select-&-insert6 0.1 (simple-select (foldl (λ(record db) (insert db "Tabela" record)) (add-table (init-database) (create-table "Tabela" '("Coloana1" "Coloana2" "Coloana3"))) (for/list ([k (in-naturals)] [x (in-range 100)] [y (in-naturals 20)]) (list (cons "Coloana1" k) (cons "Coloana2" x) (cons "Coloana3" y)))) "Tabela" '("Coloana3" "Coloana2" "Coloana1")) (list (for/list ([k (in-naturals 20)] [x (in-range 100)]) k) (for/list ([k (in-naturals)] [x (in-range 100)]) k) (for/list ([k (in-naturals 0)] [x (in-range 100)]) k)))
(check-exp-part 'simple-select-&-insert7 0.1 (simple-select (foldl (λ(record db) (insert db "Tabela" record)) (add-table (init-database) (create-table "Tabela" '("Coloana1" "Coloana2" "Coloana3" "Coloana4" "Coloana5"))) (for/list ([k (in-naturals 256)] [x (in-range 1000)] [y (in-naturals 520)]) (list (cons "Coloana1" k) (cons "Coloana2" x) (cons "Coloana3" y)))) "Tabela" '("Coloana3" "Coloana2" "Coloana1" "Coloana4" "Coloana5")) (list (for/list ([k (in-naturals 520)] [x (in-range 1000)]) k) (for/list ([k (in-naturals)] [x (in-range 1000)]) k) (for/list ([k (in-naturals 256)] [x (in-range 1000)]) k) (for/list ([x (in-range 1000)]) NULL) (for/list ([x (in-range 1000)]) NULL)))

(Task 2 : 30 puncte)
(check-exp-part 'select1 0.05 (select db "Studenți" (list "Nume" "Medie") (list (list >= "Medie" 9.9))) '(("Popescu" "Popa") (9.91 9.99)))
(check-exp-part 'select2 0.05 (select db "Studenți" (list "Nume" "Prenume" "Medie") (list (list > "Număr matricol" 124))) '(("Popa" "Georgescu") ("Ionel" "Ioana") (9.99 9.87)))
(check-exp-part 'select3 0.05 (select db "Studenți" (list "Prenume" "Nume") (list (list < "Medie" 9.9))) '(("Gigel" "Ioana") ("Ionescu" "Georgescu")))
(check-exp-part 'select4 0.05 (select db "Studenți" (list "Prenume" "Grupă" "Nume") (list (list < "Medie" 9.9) (list >= "Număr matricol" 120))) '(("Gigel" "Ioana") ("321CA" "321CD") ("Ionescu" "Georgescu")))
(check-exp-part 'select5 0.05 (select db "Cursuri" (list "Anul" "Disciplină" "Semestru") (list (list equal? "Semestru" "I") (list >= "Număr credite" 5))) '(("I" "III" "IV") ("Programarea calculatoarelor" "Algoritmi paraleli și distribuiți" "Inteligență artificială") ("I" "I" "I")))
(check-exp-part 'select6 0.05 (select db "Cursuri" (list "Anul" "Disciplină" "Semestru") (list (list equal? "Anul" "I") (list > "Număr teme" 0))) '(("I" "I") ("Programarea calculatoarelor" "Structuri de date") ("I" "II")))
(check-exp-part 'select7 0.05 (select db "Cursuri" (list "Anul" "Disciplină" "Semestru") (list (list equal? "Semestru" "I") (list > "Număr teme" 0) (list >= "Număr credite" 5))) '(("I" "III" "IV") ("Programarea calculatoarelor" "Algoritmi paraleli și distribuiți" "Inteligență artificială") ("I" "I" "I")))
(check-exp-part 'select8 0.05 (select db "Studenți" (list "Nume" (cons 'min "Medie")) '()) '(("Ionescu" "Popescu" "Popa" "Georgescu") 9.82))
(check-exp-part 'select9 0.05 (select db "Studenți" (list "Prenume" "Nume" (cons 'max "Medie")) '()) '(("Gigel" "Maria" "Ionel" "Ioana") ("Ionescu" "Popescu" "Popa" "Georgescu") 9.99))
(check-exp-part 'select10 0.05 (select db "Studenți" (list (cons 'sort-asc "Număr matricol") "Prenume" "Nume" (cons 'count "Medie")) '()) '((123 124 125 126) ("Gigel" "Maria" "Ionel" "Ioana") ("Ionescu" "Popescu" "Popa" "Georgescu") 4))
(check-exp-part 'select11 0.05 (select db "Studenți" (list "Nume" (cons 'min "Medie")) (list (list > "Medie" 9.5))) '(("Ionescu" "Popescu" "Popa" "Georgescu") 9.82))
(check-exp-part 'select12 0.05 (select db "Studenți" (list "Prenume" "Nume" (cons 'max "Medie")) (list (list >= "Număr matricol" 125))) '(("Ionel" "Ioana") ("Popa" "Georgescu") 9.99))
(check-exp-part 'select13 0.05 (select db "Studenți" (list (cons 'sort-asc "Număr matricol") "Prenume" "Nume" (cons 'count "Medie")) (list (list > "Medie" 9.85))) '((124 125 126) ("Maria" "Ionel" "Ioana") ("Popescu" "Popa" "Georgescu") 3))
(check-exp-part 'select14 0.05 (select db "Studenți" (list (cons 'avg "Număr matricol") "Prenume" "Nume" (cons 'sort-desc "Medie")) (list (list > "Medie" 9.85))) '(125 ("Maria" "Ionel" "Ioana") ("Popescu" "Popa" "Georgescu") (9.99 9.91 9.87)))
(check-exp-part 'select15 0.1 (select (foldl (λ(record db) (insert db "Tabela" record)) (add-table (init-database) (create-table "Tabela" '("Coloana1" "Coloana2" "Coloana3" "Coloana4" "Coloana5"))) (for/list ([k (in-naturals 256)] [x (in-range 10)] [y (in-naturals 520)]) (list (cons "Coloana1" k) (cons "Coloana2" x) (cons "Coloana3" y) (cons "Coloana5" (+ x y)) (cons "Coloana4" (- x y))))) "Tabela" (list (cons 'min "Coloana3") (cons 'count "Coloana2") (cons 'sort-asc "Coloana1") (cons 'sort-desc "Coloana4") (cons 'sum "Coloana5")) '()) '(520 10 (256 257 258 259 260 261 262 263 264 265) (-520 -520 -520 -520 -520 -520 -520 -520 -520 -520) 5290))
(check-exp-part 'select16 0.1 (select (foldl (λ(record db) (insert db "Tabela" record)) (add-table (init-database) (create-table "Tabela" '("Coloana1" "Coloana2" "Coloana3" "Coloana4" "Coloana5"))) (for/list ([k (in-naturals 256)] [x (in-range 10)] [y (in-naturals 520)]) (list (cons "Coloana1" k) (cons "Coloana2" x) (cons "Coloana3" y) (cons "Coloana5" (+ x y)) (cons "Coloana4" (+ (- x y) k))))) "Tabela" (list (cons 'min "Coloana3") (cons 'count "Coloana2") (cons 'sort-asc "Coloana1") (cons 'sort-desc "Coloana4") (cons 'sum "Coloana5")) (list (list > "Coloana1" 260) (list < "Coloana4" -257)))  '(525 2 (261 262) (-258 -259) 1062))
(check-exp-part 'select17 0.1 (select (foldl (λ(record db) (insert db "Tabela" record)) (add-table (init-database) (create-table "Tabela" '("Coloana1" "Coloana2" "Coloana3" "Coloana4" "Coloana5"))) (for/list ([k (in-naturals 256)] [x (in-range 1000)] [y (in-naturals 520)]) (list (cons "Coloana1" k) (cons "Coloana2" x) (cons "Coloana3" y) (cons "Coloana5" (+ x y)) (cons "Coloana4" (- x y))))) "Tabela" (list (cons 'min "Coloana3") (cons 'max "Coloana2") (cons 'avg "Coloana1") (cons 'count "Coloana4") (cons 'sum "Coloana5")) (list (list > "Coloana1" 260))) '(525 999 758 1 1516380))

      

(Task 3 : 20 puncte)
(check-exp-part 'update1 0.1 (simple-select (update db "Studenți" (list (cons "Medie" 10)) (list (list >= "Medie" 9.5))) "Studenți" '("Nume" "Prenume" "Medie")) '(("Ionescu" "Popescu" "Popa" "Georgescu") ("Gigel" "Maria" "Ionel" "Ioana") (10 10 10 10)))
(check-exp-part 'update2 0.1 (simple-select (update db "Cursuri" (list (cons "Număr teme" 3)) (list (list < "Număr teme" 3) (list equal? "Anul" "I"))) "Cursuri" '("Disciplină" "Număr teme" "Anul")) '(("Programarea calculatoarelor" "Paradigme de programare" "Algoritmi paraleli și distribuiți" "Inteligență artificială" "Structuri de date" "Baze de date") (3 3 3 3 3 0) ("I" "II" "III" "IV" "I" "III")))
(check-exp-part 'update3 0.1  (simple-select (update (update db "Cursuri" (list (cons "Număr teme" 3)) (list (list < "Număr teme" 3) (list equal? "Anul" "I"))) "Cursuri" (list (cons "Număr credite" 6) (cons "Semestru" "II")) (list (list equal? "Semestru" "I")))  "Cursuri" '("Anul" "Semestru" "Număr credite" "Număr teme")) '(("I" "II" "III" "IV" "I" "III") ("II" "II" "II" "II" "II" "II") (6 6 6 6 5 5) (3 3 3 3 3 0)))
(check-exp-part 'update4 0.15 (select (update (update db "Cursuri" (list (cons "Număr teme" 3)) (list (list < "Număr teme" 3) (list equal? "Anul" "I"))) "Cursuri" (list (cons "Număr credite" 6) (cons "Anul" "I") (cons "Semestru" "II")) (list (list equal? "Semestru" "I")))  "Cursuri" (list (cons 'count "Anul") (cons 'count "Semestru") (cons 'sum "Număr credite") (cons 'sort-desc "Număr teme")) (list (list > "Număr credite" 0))) '(3 1 34 (3 3 3 3 3 0)))
(check-exp-part 'update5 0.15 (select (update (foldl (λ(record db) (insert db "Tabela" record)) (add-table (init-database) (create-table "Tabela" '("Coloana1" "Coloana2" "Coloana3"))) (for/list ([k (in-naturals)] [x (in-range 100)] [y (in-naturals 20)]) (list (cons "Coloana1" k) (cons "Coloana2" x) (cons "Coloana3" y)))) "Tabela" (list (cons "Coloana1" 222) (cons "Coloana2" 694)) (list (list < "Coloana2" 80) (list > "Coloana1" 5))) "Tabela" (list (cons 'count "Coloana1") (cons 'sum "Coloana2")) '()) '(27 53161))
(check-exp-part 'update6 0.2 (select (update (foldl (λ(record db) (insert db "Tabela" record)) (add-table (init-database) (create-table "Tabela" '("Coloana1" "Coloana2" "Coloana3"))) (for/list ([k (in-naturals)] [x (in-range 100)] [y (in-naturals 20)]) (list (cons "Coloana1" k) (cons "Coloana2" x) (cons "Coloana3" y)))) "Tabela" (list (cons "Coloana1" 222) (cons "Coloana2" 694)) (list (list < "Coloana2" 80) (list > "Coloana1" 5))) "Tabela" (list (cons 'count "Coloana1") (cons 'sum "Coloana2")) '()) '(27 53161))
(check-exp-part 'update7 0.2 (select (update (foldl (λ(record db) (insert db "Tabela" record)) (add-table (init-database) (create-table "Tabela" '("Coloana1" "Coloana2" "Coloana3"))) (for/list ([k (in-naturals)] [x (in-range 100)] [y (in-naturals 20)]) (list (cons "Coloana1" k) (cons "Coloana2" x) (cons "Coloana3" y)))) "Tabela" (list (cons "Coloana1" 128) (cons "Coloana2" 542) (cons "Coloana5" 452)) (list (list < "Coloana2" 80) (list > "Coloana1" 5))) "Tabela" (list (cons 'count "Coloana1") (cons 'min "Coloana2") (cons 'max "Coloana3")) (list (list > "Coloana1" 100))) '(1 542 99))

(Task 4 : 10 puncte)
(check-exp-part 'delete1 0.1 (simple-select (delete db "Studenți" (list (list < "Medie" 9.9))) "Studenți" '("Nume" "Prenume")) '(("Popescu" "Popa") ("Maria" "Ionel")))
(check-exp-part 'delete2 0.1 (simple-select (delete db "Studenți" (list (list >= "Medie" 9.9))) "Studenți" '("Nume" "Prenume" "Medie")) '(("Ionescu" "Georgescu") ("Gigel" "Ioana") (9.82 9.87)))
(check-exp-part 'delete3 0.1 (simple-select (delete db "Cursuri" (list (list equal? "Anul" "I") (list equal? "Semestru" "I"))) "Cursuri" '("Disciplină" "Semestru" "Număr teme")) '(("Paradigme de programare" "Algoritmi paraleli și distribuiți" "Inteligență artificială" "Structuri de date" "Baze de date") ("II" "I" "I" "II" "II") (3 3 3 3 0)))
(check-exp-part 'delete4 0.1 (simple-select (delete db "Studenți" (list (list < "Medie" 5) (list equal? "Nume" "Popescu"))) "Studenți" '("Nume" "Prenume" "Medie")) '(("Ionescu" "Popescu" "Popa" "Georgescu") ("Gigel" "Maria" "Ionel" "Ioana") (9.82 9.91 9.99 9.87)))
(check-exp-part 'delete5 0.1 (simple-select (delete db "Studenți" (list (list >= "Medie" 9.8) (list equal? "Nume" "Popescu"))) "Studenți" '("Nume" "Prenume" "Medie")) '(("Ionescu" "Popa" "Georgescu") ("Gigel" "Ionel" "Ioana") (9.82 9.99 9.87)))
(check-exp-part 'delete6 0.1 (simple-select (delete db "Cursuri" (list (list equal? "Anul" "I") (list >= "Număr credite" 5))) "Cursuri" '("Semestru" "Disciplină" "Număr teme" "Număr credite")) '(("II" "I" "I" "II") ("Paradigme de programare" "Algoritmi paraleli și distribuiți" "Inteligență artificială" "Baze de date") (3 3 3 0) (6 5 6 5)))
(check-exp-part 'delete7 0.1 (simple-select (delete db "Cursuri" (list (list >= "Număr credite" 5))) "Cursuri" '("Semestru" "Disciplină" "Număr teme" "Număr credite")) '())
(check-exp-part 'delete8 0.1 (simple-select (delete db "Cursuri" (list (list equal? "Anul" "I") (list equal? "Semestru" "I") (list > "Număr credite" 5) )) "Cursuri" '("Semestru" "Disciplină" "Număr teme" "Număr credite")) '(("I" "II" "I" "I" "II" "II") ("Programarea calculatoarelor" "Paradigme de programare" "Algoritmi paraleli și distribuiți" "Inteligență artificială" "Structuri de date" "Baze de date") (2 3 3 3 3 0) (5 6 5 6 5 5)))
(check-exp-part 'delete9 0.1  (simple-select (delete (foldl (λ(record db) (insert db "Tabela" record)) (add-table (init-database) (create-table "Tabela" '("Coloana1" "Coloana2" "Coloana3"))) (for/list ([k (in-naturals)] [x (in-range 100)] [y (in-naturals 20)]) (list (cons "Coloana1" k) (cons "Coloana2" x) (cons "Coloana3" y)))) "Tabela" (list (list < "Coloana2" 100) (list > "Coloana1" 5))) "Tabela" '("Coloana3" "Coloana2" "Coloana1")) '((20 21 22 23 24 25) (0 1 2 3 4 5) (0 1 2 3 4 5))) 
(check-exp-part 'delete10 0.1 (simple-select (delete (foldl (λ(record db) (insert db "Tabela" record)) (add-table (init-database) (create-table "Tabela" '("Coloana1" "Coloana2" "Coloana3"))) (for/list ([k (in-naturals)] [x (in-range 100)] [y (in-naturals 20)]) (list (cons "Coloana1" k) (cons "Coloana2" x) (cons "Coloana3" y)))) "Tabela" (list (list < "Coloana2" 80) (list > "Coloana1" 5))) "Tabela" '("Coloana1")) '((0 1 2 3 4 5 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99)))

(sumar)