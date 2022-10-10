; заготовка "Доктора" + выполненные задания. Сентябрь 2022
#lang scheme/base
(require racket/vector)

; В учебных целях используется базовая версия Scheme

; основная функция, запускающая "Доктора"
; параметр name -- имя пациента
(define (visit-doctor name)
  (printf "Hello, ~a!\n" name)
  (print '(what seems to be the trouble?))
  (doctor-driver-loop-v2 name)
)


; цикл диалога Доктора с пациентом
; параметр name -- имя пациента
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
      (newline)
      (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
      (let ((user-response (read)))
        (cond 
        ((equal? user-response '(goodbye)) ; реплика '(goodbye) служит для выхода из цикла
              (printf "Goodbye, ~a!\n" name)
              (print '(see you next week)))
              (else (print (reply user-response history-vctr)) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
                    (loop name (vector-append history-vctr (vector user-response)))
              )
        )
        )
    )
))


; генерация ответной реплики по user-response -- реплике от пользователя 
(define (reply user-response history-vctr)
      (case (random 0 (if (equal? history-vctr #()) 2 3)) ; с равной вероятностью выбирается один из трёх способов построения ответа.
                                                          ; однако если массив ответов пользователя пуст, можно выбрать только 1й или 2й способ 
          ((0) (hedge-answer)) ; 1й способ
          ((1) (qualifier-answer user-response)) ; 2й способ
          ((2) (history-answer history-vctr)) ; 3й способ
      )
)


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

(visit-doctor 'ivan)
