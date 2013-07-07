
(ql:quickload :mini-sdl2)
(ql:quickload :cl-opengl)

(in-package :cl-user)
(defpackage :test-sdl2
  (:use :cl :sdl2))
(in-package :test-sdl2)

(with-sdl2 (:video :audio :joystick)
  (with-window-and-context (win ctx :title "HOGE" :flags (:opengl))
    (format t "Num Joysticks : ~A~%" (num-joysticks))
    (joystick-open 0)
    (if (joystick-opened 0) (format t "JoystickOpened~%"))
    (loop-event-handling event-catch
      :window
      (with-event-slot :window (event-catch :window-id id
                                            :win-event winevent
                                            :data1 d1
                                            :data2 d2)
        (format t "~S~%" (list id winevent d1 d2)))
      :keyboard
      (with-event-slot :keyboard (event-catch :window-id id
                                              :state state
                                              :repeat repeat
                                              :keysym sym)
        (format t "~S~%" (list id state repeat sym)))
      :mouse-motion
      (with-event-slot :mouse-motion (event-catch :window-id id
                                                  :state state
                                                  :x x
                                                  :y y)
        (format t "~S~%" (list id state x y)))
      :mouse-button
      (with-event-slot :mouse-button (event-catch :window-id id
                                                  :state state
                                                  :button button
                                                  :x x
                                                  :y y)
        (format t "~S~%" (list id state button x y)))
      :text-editing
      (with-event-slot :text-editing (event-catch :window-id id
                                                  :text text
                                                  :start start
                                                  :length length)
        (format t "~S~%" (list id text start length)))
      :text-input
      (with-event-slot :text-input (event-catch :window-id id
                                                :text text)
        (format t "~S~%" (list id text)))
      :joy-axis
      (with-event-slot :joy-axis (event-catch :which which
                                              :axis axis
                                              :value value)
        (format t "~S~%" (list which axis value)))
      :joy-ball
      (with-event-slot :joy-ball (event-catch :which which
                                              :ball ball
                                              :xrel x
                                              :yrel y)
        (format t "~S~%" (list which ball x y)))
      :joy-hat
      (with-event-slot :joy-hat (event-catch :which which
                                             :hat hat
                                             :value value)
        (format t "~S~%" (list which hat value)))
      :joy-button
      (with-event-slot :joy-button (event-catch :which which
                                                :button button
                                                :state state)
        (format t "~S~%" (list which button state)))
      :quit
      (leave-event-loop)
      :idle
      (begin-frame win ctx
        (gl:clear-color 1.0 0.0 0.0 1.0)
        (gl:clear :color-buffer-bit)))))

