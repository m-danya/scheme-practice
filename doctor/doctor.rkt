; заготовка "Доктора" + выполненные задания. Сентябрь 2022
#lang scheme/base
(require racket/vector)

; В учебных целях используется базовая версия Scheme

; основная функция, запускающая "Доктора"
; параметр name -- имя пациента
; (неиспользуемая версия)
(define (visit-doctor name)
  (printf "Hello, ~a!\n" name)
  (print '(what seems to be the trouble?))
  (doctor-driver-loop-v2 name)
)


; основная функция, запускающая "Доктора"
; параметр stop-word - стоп-слово, после которого доктор прекращает работу (вводится вместо имени)
; параметр max-patients-number - максималное количество пациентов, после которого доктор прекращает работу
(define (visit-doctor-v2 stop-word max-patients-number) (
  if (< max-patients-number 1) (say-goodbye) (
    let ((name (ask-patient-name))) (
      if (equal? name stop-word) (say-goodbye) (
        begin
        (printf "Hello, ~a!\n" name)
        (print '(what seems to be the trouble?))
        (doctor-driver-loop-v2 name)
        (visit-doctor-v2 stop-word (- max-patients-number 1))
  )))
))


; функция, которая запрашивает имя пациента
(define (ask-patient-name)
 (begin
  (println '(next!))
  (println '(who are you?))
  (print '**)
  (car (read))
 ) 
)


; функция, выводящая текст прищания
(define (say-goodbye) (
  println '(time to go home)
))


; цикл диалога Доктора с пациентом
; параметр name -- имя пациента
; (неиспользуемая версия)
(define (doctor-driver-loop name)
    (newline)
    (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
    (let ((user-response (read)))
      (cond 
	    ((equal? user-response '(goodbye)) ; реплика '(goodbye) служит для выхода из цикла
             (printf "Goodbye, ~a!\n" name)
             (print '(see you next week)))
            (else (print (reply user-response)) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
                  (doctor-driver-loop name)
             )
       )
      )
)


; цикл диалога Доктора с пациентом
; параметр name -- имя пациента
; сохраняет список реплик пользователя
(define (doctor-driver-loop-v2 name) (
    let loop ((name name) (history-vctr #())) (
      begin
      (newline)
      (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
      (let ((user-response (read)))
        (cond 
        ((equal? user-response '(goodbye)) ; реплика '(goodbye) служит для выхода из цикла
              (printf "Goodbye, ~a!\n" name)
              (println '(see you next week)))
              (else (print (reply user-response history-vctr)) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
                    (loop name (vector-append history-vctr (vector user-response)))
              )
        )
        )
    )
))


; генерация ответной реплики по user-response -- реплике от пользователя 
(define (reply user-response history-vctr)
      (case (random 
              (if (can-use-keyword-answer user-response) -1 0)
              (if (equal? history-vctr #()) 2 3)
            )
          ; с равной вероятностью выбирается один из четыерёх способов построения ответа.
          ; однако, если массив ответов пользователя пуст, выбрать последний вариант нельзя,
          ; а если в ответет пользователя нет ключевых слов, нельзя применить первый (4й)
          ((-1) (keyword-answer user-response)) ; 4й способ
          ((0) (hedge-answer)) ; 1й способ
          ((1) (qualifier-answer user-response)) ; 2й способ
          ((2) (history-answer history-vctr)) ; 3й способ
      )
)


(define (can-use-keyword-answer user-response) (
  ; можно переписать через foldl + call/cc, получится более громоздко
  if (null? user-response) #f (
    if (has-vector-elem? keywords-lst (car user-response)) #t (
      can-use-keyword-answer (cdr user-response)
    )
  )
))


(define (has-vector-elem? vctr elem) (
  ; можно переписать через foldl + call/cc, получится более громоздко
  let ((vctr-length (vector-length vctr))) (
    let loop ((idx 0)) (
      if (>= idx vctr-length) #f (
        if (equal? elem (vector-ref vctr idx)) #t (
          loop (+ idx 1)
        )
    )
  )
  )
))

; 1й способ генерации ответной реплики -- случайный выбор одной из заготовленных фраз, не связанных с репликой пользователя
(define (hedge-answer)
       (pick-random-vector '#((please go on)
                              (many people have the same sorts of feelings)
                              (many of my patients have told me the same thing)
                              (please continue)
                              (sounds like something significant to you)
                              (can you tell me more about this?)
                              (how do you feel about what you have told me?)
                              )
         )
)


; случайный выбор одного из элементов непустого вектора
(define (pick-random-vector vctr)
  (vector-ref vctr (random 0 (vector-length vctr)))
)


; случайный выбор одного из элементов непустого списка
(define (pick-random-list lst)
  (list-ref lst (random 0 (length lst)))
)


; 2й способ генерации ответной реплики -- замена лица в реплике пользователя и приписывание к результату случайно выбранного нового начала
(define (qualifier-answer user-response)
        (append (pick-random-vector '#((you seem to think that)
                                       (you feel that)
                                       (why do you believe that)
                                       (why do you say that)
                                       (are you sure that)
                                       (how come)
                                       (how much do you care that)
                                       )
                )
                (change-person user-response)
        )
 )

; 3й способ генерации ответной реплики — использовать фразу, которую пользователь когда-то сказал

(define (history-answer history-vctr) (
  append '(earlier you said that) (
    change-person
      (pick-random-vector history-vctr)
  )
))

; 4й способ генерации ответной реплики — найти ключевое слово и выбрать ответ из ассоц. списка 
; с заменой "*" на ключевое слово

(define (keyword-answer user-response) (
  let* (
    (
      chosen-keyword
      (pick-random-list (leave-keywords-only user-response))
    )
    (chosen-template (
      pick-random-vector (get-templates-by-keyword chosen-keyword)
    ))
  ) (
    many-replace-v3
      (list (list '* chosen-keyword))
      chosen-template
  )
))

; фильтрация входной строки, которая оставляет только слова, являющиеся ключевыми
(define (leave-keywords-only user-response) (
  filter (lambda (word) (has-vector-elem? keywords-lst word)) user-response
))


; получения единого вектора всех применимых фраз-шаблонов для данного ключевого слова
(define (get-templates-by-keyword keyword) (
  vector-foldl
  (lambda (idx result x) (
    vector-append result x
  ))
  #()
  (
    vector-map
      (lambda (v) (vector-ref v 1))
      (vector-filter
        (lambda (t) (
          has-vector-elem? (vector-ref t 0) keyword
        ))
        keywords-structure
      )
  )
))

; замена лица во фразе
(define (change-person phrase)
        (many-replace-v3
		  '((am are)
        (are am)
        (i you)
        (me you)
        (mine yours)
        (my your)
        (myself yourself)
        (you i)
        (your my)
        (yours mine)
        (yourself myself)
        (we you)
        (us you)
        (our your)
        (ours yours)
        (ourselves yourselves)
        (yourselves ourselves)
        (shall will))
                      phrase)
 )


; осуществление всех замен в списке lst по ассоциативному списку replacement-pairs
; (порождаемый процесс рекурсивен)
; (неиспользуемая версия)
(define (many-replace replacement-pairs lst)
        (cond ((null? lst) lst)
              (else (let ((pat-rep (assoc (car lst) replacement-pairs))) ; Доктор ищет первый элемент списка в ассоциативном списке замен
                      (cons (if pat-rep (cadr pat-rep) ;  если поиск был удачен, то в начало ответа Доктор пишет замену
                                (car lst) ; иначе в начале ответа помещается начало списка без изменений
                            )
                            (many-replace replacement-pairs (cdr lst)) ; рекурсивно производятся замены в хвосте списка
                        )
                     )
               )
         )
)


; осуществление всех замен в списке lst по ассоциативному списку replacement-pairs
; (порождаемый процесс итеративен)
; (неиспользуемая версия)
(define (many-replace-v2 replacement-pairs lst) (
  let loop ((lst lst) (result '())) (
    if (null? lst) (reverse result) ( ; поскольку полученный результат для эффективности строился
                                      ; задом наперёд, его нужно перевернуть
      let ((pat-rep (assoc (car lst) replacement-pairs))) ( ; Доктор ищет первый элемент списка в ассоциативном списке замен
        loop (cdr lst) (
          cons (
                  if pat-rep (cadr pat-rep) ;  если поиск был удачен, то в начало result Доктор пишет замену
                  (car lst) ; иначе в начало result помещается начало списка без изменений
               ) result
        )
      )
    )
  )
))

; осуществление всех замен в списке lst по ассоциативному списку replacement-pairs
(define (many-replace-v3 replacement-pairs lst) (
  map
    (lambda (word) (
      let ((pat-rep (assoc word replacement-pairs))) ( ; Доктор ищет первый элемент списка в ассоциативном списке замен
        if pat-rep (cadr pat-rep) ;  если поиск был удачен, то в слово заменяется на нужное
                    word          ;  иначе слово остаётся тем же
      )
    ))
    lst
))


; в Racket нет vector-foldl, реализуем для случая с одним вектором (vect-foldl f init vctr)
; у f три параметра i -- индекс текущего элемента, result -- текущий результат свёртки, elem -- текущий элемент вектора
(define (vector-foldl f init vctr)
 (let ((length (vector-length vctr)))
  (let loop ((i 0) (result init))
   (if (= i length) result
    (loop (add1 i) (f i result (vector-ref vctr i)))))))


; аналогично от конца вектора к началу
(define (vector-foldr f init vctr)
 (let ((length (vector-length vctr)))
  (let loop ((i (sub1 length)) (result init))
   (if (= i -1) result
    (loop (sub1 i) (f i result (vector-ref vctr i)))))))


; структура с шаблонами
(define keywords-structure '#(
  #( ; начало данных 1й группы
    #(depressed suicide exams university) ; список ключевых слов 1й группы
    #( ; список шаблонов для составления ответных реплик 1й группы 
	    (when you feel depressed, go out for ice cream)
      (depression is a disease that can be treated)
      (try to do things that make you happy)
      (chocolate is known to be useful when you feel sad)
	)
  ) ; завершение данных 1й группы
  #( ; начало данных 2й группы ...
    #(mother father parents brother sister uncle aunt grandma grandpa)
    #(
	    (tell me more about your * i want to know all about your *)
      (why do you feel that way about your * ?)
      (what kind of relationship do you have with your * ?)
      (what about your * ?)
	)
  )
  #(
    #(university scheme lections study)
    #(
      (your education is important)
      (how much time do you spend on your studies ?)
      (sometimes studying can be tough)
      (do you have diffuculties with your * ?)
    )
	)
  #(
    #(worthless useless talentless self-assessment esteem)
    #(
      (where are these thoughts coming from ?)
      (you are better than you think of yourself)
    )
  )
  #(
    #(afraid anxiety scared)
    #(
      (try to concentrate on your breath when you feel *)
      (take a break from what is bothering you)
    )
  )
))


; список ключевых слов, получаемый из структуры выше
(define keywords-lst (
  vector-foldl (lambda (index result t) (vector-append result (vector-ref t 0))) '#() keywords-structure
))


(visit-doctor-v2 'suppertime 3)
