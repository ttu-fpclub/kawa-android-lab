
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
  (let ((str ""))
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
           ; CODE This will be called whenever any of the buttons is clicked. It should react
           ;      accordingly.
           ())
          ; Updates the text and stack fields on the screen with the current values in the instance.
          ((updateFields)
           ((android.os.Handler):post (lambda ()
                                        ; CODE This needs to take the values in the instance variables text and
                                        ;      stack and make them show up on the screen in the right places.
                                        ())))
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
           ; CODE This method needs to perform the operation given, if it can.
           ())
          ; Moves the current text to the stack and empties the current text.
          ((enterKey)
           ; CODE This method needs to perform the enter key (=) action.
           ())
          ; This is where the magic happens. When the view gets created, we want to add all the button
          ; listeners so that they know what to do when clicked. Each listener will be a 0-ary lambda
          ; that modifies the state of the view.
          (on-create-view
           ; CODE All of these lambdas will end up having content, corresponding to what each button is
           ;      intended to do.
           (addListener R:id:sto   (lambda () ()))
           (addListener R:id:rtrv  (lambda () ()))
           (addListener R:id:pop   (lambda () ()))
           (addListener R:id:swap  (lambda () ()))
           (addListener R:id:back  (lambda () ()))
           (addListener R:id:_0    (lambda () ()))
           (addListener R:id:_1    (lambda () ()))
           (addListener R:id:_2    (lambda () ()))
           (addListener R:id:_3    (lambda () ()))
           (addListener R:id:_4    (lambda () ()))
           (addListener R:id:_5    (lambda () ()))
           (addListener R:id:_6    (lambda () ()))
           (addListener R:id:_7    (lambda () ()))
           (addListener R:id:_8    (lambda () ()))
           (addListener R:id:_9    (lambda () ()))
           (addListener R:id:add   (lambda () ()))
           (addListener R:id:sub   (lambda () ()))
           (addListener R:id:mul   (lambda () ()))
           (addListener R:id:div   (lambda () ()))
           (addListener R:id:sin   (lambda () ()))
           (addListener R:id:cos   (lambda () ()))
           (addListener R:id:tan   (lambda () ()))
           (addListener R:id:neg   (lambda () ()))
           (addListener R:id:dec   (lambda () ()))
           (addListener R:id:eval  (lambda () ()))
           R:layout:main))
