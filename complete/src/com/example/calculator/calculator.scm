
; android-defs gives us the activity macro and several shortened names. It's not strictly necessary, but
; it certainly comes in handy.
(require 'android-defs)

; These two import standard Scheme library functions that we'll be using.
(require 'list-lib)
(import (rnrs hashtables))

; When we use a Java class name a lot, we want to alias it so we can use its short name. This is about
; like writing an 'import' statement if we were in Java.
(define-alias R com.example.calculator.R)
(define-alias Activity android.app.Activity)
(define-alias Toast android.widget.Toast)

; Nice convenience function to convert the stack to a string, with the bottom element on the left-hand side.
(define (pretty-print-stack stack) :: string
  (let ((str " "))
    (for-each (lambda (x)
                (set! str (string-append (x:toString) " " str)))
              stack)
    str))

; Display a message to the user.
(define (display act :: Activity str :: string) :: void
  ((Toast:makeText act str Toast:LENGTH_SHORT):show))

; Display the "Too few elements on stack" message.
(define (too-few act :: Activity) :: void
  (display act "Too few elements on stack "))

; This is a macro that will check to make sure the stack has enough arguments on it. For example, if we
; hit the addition (+) button and there are fewer than two arguments on the stack, we want to refuse to
; perform the addition and instead show a message.
(define-syntax assert-stack
  (syntax-rules ()
    ((_ act count body ...)
     (if (>= (act:stackSize) count)
         (begin body ...)
         (too-few act)))))

; activity is a macro defined in android-defs which creates a subclass of android.app.Activity.
(activity calculator
          ; Instance variables. Note that some of them have type annotations; this is sometimes necessary
          ; if Java needs to disambiguate (for example, if you pass the value to a Java method, it needs
          ; to know the compile-time type).
          (stack :: list ())
          (text :: string "")
          (variable :: real 0.0)
          (responses (make-eqv-hashtable 25))
          ; General receiver for click events. There's nothing special about the name onClick. The layout
          ; file defined each button as reporting its click event to this method.
          ((onClick view :: View) :: void
           ; When a button is clicked, we want to look up what to do in the hashtable and then call that
           ; procedure. Afterward, we will update the text fields automatically.
           (let ((ref (hashtable-ref responses (view:getId) #f)))
             (when ref
                   (ref))
             (updateFields)))
          ; Updates the text and stack fields on the screen with the current values in the instance.
          ((updateFields)
           ((android.os.Handler):post (lambda ()
                                        (let ((text-field (->TextView (findViewById R:id:field)))
                                              (stack-field (->TextView (findViewById R:id:stack))))
                                          (text-field:setText text)
                                          (stack-field:setText (pretty-print-stack stack))))))
          ; Convenience function to add an element to the hashtable.
          ((addListener id func)
           (hashtable-set! responses id func))
          ; Appends text onto the end of the current input.
          ((addText ch)
           (set! text (string-append text ch)))
          ; Checks to see if the number contains a decimal point.
          ((hasDecimal)
           (any (lambda (x) (eqv? x #\.)) (string->list text)))
          ; Returns the size of the stack.
          ((stackSize)
           (length stack))
          ; Pushes an element onto the stack.
          ((pushStack x)
           (set! stack (cons x stack)))
          ; Pops an element off the stack. Returns #f if empty.
          ((popStack)
           (if (null? stack)
               #f
               (let ((top (car stack)))
                 (set! stack (cdr stack))
                 top)))
          ; Performs an operation on the top n stack elements.
          ((operation count op)
           (enterKey)
           (assert-stack (this) count
                         (letrec ((pop-all (lambda (n arr)
                                             (if (zero? n)
                                                 arr
                                                 (pop-all (- n 1) (cons (popStack) arr))))))
                           (pushStack (apply op (pop-all count ()))))))
          ; Moves the current text to the stack and empties the current text.
          ((enterKey)
           (let ((new-value (try-catch (java.lang.Double:parseDouble text)
                                       (ex java.lang.NumberFormatException #f))))
             (when new-value
                   (pushStack new-value)
                   (set! text ""))))
          ; This is where the magic happens. When the view gets created, we want to add all the button
          ; listeners so that they know what to do when clicked. Each listener will be a 0-ary lambda
          ; that modifies the state of the view.
          (on-create-view
           (addListener R:id:sto   (lambda () (begin (enterKey)
                                                     (assert-stack (this) 1
                                                                   (set! variable (popStack))))))
           (addListener R:id:rtrv  (lambda () (set! text (variable:toString))))
           (addListener R:id:pop   (lambda () (assert-stack (this) 1
                                                            (set! text ((popStack):toString)))))
           (addListener R:id:swap  (lambda () (assert-stack (this) 2
                                                            (let ((x1 (popStack))
                                                                  (x2 (popStack)))
                                                              (pushStack x1)
                                                              (pushStack x2)))))
           (addListener R:id:back  (lambda () (set! text "")))
           (addListener R:id:_0    (lambda () (addText "0")))
           (addListener R:id:_1    (lambda () (addText "1")))
           (addListener R:id:_2    (lambda () (addText "2")))
           (addListener R:id:_3    (lambda () (addText "3")))
           (addListener R:id:_4    (lambda () (addText "4")))
           (addListener R:id:_5    (lambda () (addText "5")))
           (addListener R:id:_6    (lambda () (addText "6")))
           (addListener R:id:_7    (lambda () (addText "7")))
           (addListener R:id:_8    (lambda () (addText "8")))
           (addListener R:id:_9    (lambda () (addText "9")))
           (addListener R:id:add   (lambda () (operation 2 +)))
           (addListener R:id:sub   (lambda () (operation 2 -)))
           (addListener R:id:mul   (lambda () (operation 2 *)))
           (addListener R:id:div   (lambda () (operation 2 /)))
           (addListener R:id:sin   (lambda () (operation 1 sin)))
           (addListener R:id:cos   (lambda () (operation 1 cos)))
           (addListener R:id:tan   (lambda () (operation 1 tan)))
           (addListener R:id:neg   (lambda () (cond
                                               ((zero? (string-length text)))
                                               ((equal? (substring text 0 1) "-")
                                                (set! text (substring text 1 (string-length text))))
                                               (else
                                                (set! text (string-append "-" text))))))
           (addListener R:id:dec   (lambda () (unless (hasDecimal)
                                                      (addText "."))))
           (addListener R:id:eval  (lambda () (enterKey)))
           (addListener R:id:pi (lambda ()
                                  (enterKey)
                                  (pushStack java.lang.Math:PI)))
           R:layout:main))

