(defpackage todo
  (:use #:cl)
  (:import-from #:slynk)
  
  (:import-from #:clack.handler.hunchentoot)
  (:import-from #:weblocks/widget
                #:render
                #:update
                #:defwidget)
  (:import-from #:weblocks/actions
                #:make-js-action)
  (:import-from #:weblocks/app
                #:defapp)
  (:import-from #:weblocks/server)
  (:import-from #:find-port)
  (:import-from #:weblocks/html
                #:with-html)
  (:import-from #:weblocks-ui/form
                #:with-html-form)
  (:import-from #:todo/toast)
  (:export #:start-server))
(in-package todo)


(defapp tasks
  :prefix "/")


(defwidget task ()
  ((title
    :initarg :title
    :accessor title)
   (done
    :initarg :done
    :initform nil
    :accessor done)))


(defun make-task (title &key done)
  (make-instance 'task :title title :done done))


(defwidget task-list ()
  ((tasks
    :initarg :tasks
    :accessor tasks)))


(defun toggle (task)
  (setf (done task)
        (if (done task)
            nil
            t))
  (update task))


(defmethod render ((task task))
  (with-html
    (:p (:input :type "checkbox"
                :checked (done task)
                :onclick (make-js-action
                          (lambda (&key &allow-other-keys)
                            (toggle task))))
        (:span (if (done task)
                   (with-html
                     ;; strike
                     (:s (title task)))
                   (title task))))))


(defmethod render ((task-list task-list))
  (with-html
    (:h1 "Tasks")
    (loop for task in (tasks task-list) do
      (render task))
    
    (with-html-form (:POST (lambda (&key title &allow-other-keys)
                             (add-task task-list title)))
      (:input :type "text"
              :name "title"
              :placeholder "Task's title")
      (:input :type "submit"
              :value "Add"))))


(defun make-task-list (&rest rest)
  (let ((tasks (loop for title in rest
                     collect (make-task title))))
    (make-instance 'task-list :tasks tasks)))


(defmethod weblocks/session:init ((app tasks))
  (declare (ignorable app))
  (make-task-list "Make my first Weblocks app"
                  "Deploy it somewhere"
                  "Have a profit"))


(defun add-task (task-list title)
  (push (make-task title)
        (tasks task-list))
  (update task-list)
  (todo/toast:raise-a-toast
   (format nil "Task \"~A\" was added"
           title)))


(defvar slynk:*use-dedicated-output-stream*)

(defun start-server ()
  (let ((port (find-port:find-port))
        (interface "localhost")
        (slynk-port 10405)
        (slynk-interface "localhost"))
    (weblocks/server:start :port port
                           :interface interface)

    ;; To make it possible to connect to a remote SLYNK server where ports are closed
    ;; with firewall.
    (setf slynk:*use-dedicated-output-stream* nil)
    
    (format t "Starting slynk server on ~A:~A (dedicated-output: ~A)~%"
            slynk-interface
            slynk-port
            slynk:*use-dedicated-output-stream*)
    
    (slynk:create-server :dont-close t
                         :port slynk-port
                         :interface slynk-interface)

    (sleep 5)
    ;; This does not work for some reason.
    ;; Have to figure out why:
    ;; (loop while (not (find-port:port-open-p port :interface interface))
    ;;       do (sleep 0.1))
    
    
    (format nil "http://~A:~A/"
            interface
            port)))
