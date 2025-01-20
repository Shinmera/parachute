(asdf:load-system :staple-markdown)
(defmethod staple:subsystems ((_ (eql (asdf:find-system :parachute)))) ())
