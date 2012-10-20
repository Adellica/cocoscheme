

(define (add-helper-labels scene)
  (begin
    (define label (CCLabelTTF::create
                   "Cocos2D-X with
Chicken Scheme
prototype"
                   "arial" 32))
    (setPosition label 200 700)
    (addChild scene label))
  
  (begin
    (define label (CCLabelTTF::create
                   "touch screen:
right-half: forward
left-half: backward"
                   "arial" 24))
    (setPosition label -150 700)
    (addChild scene label))

  (begin
    (define label (CCLabelTTF::create "Chicken Scheme REPL
 on port 1234" "arial" 24))
    (setPosition label 350 450)
    (addChild scene label))

  (begin
    (define label (CCLabelTTF::create
                   "adb forward tcp:1234 tcp:1234"
                   "arial" 24))
    (setPosition label 300 310)
    (addChild scene label))

  (begin
    (define label (CCLabelTTF::create
                   "nc localhost 1234"
                   "arial" 24))
    (setPosition label 400 275)
    (addChild scene label))

  (begin
    (define label (CCLabelTTF::create
                   "(use chickmunk)\n(body-get-pos truck)\n(reset-space-with-truck)"
                   "arial" 24))
    (setPosition label 500 200)
    (addChild scene label)))


