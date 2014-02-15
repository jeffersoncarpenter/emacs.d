(setq ty-functors ())


(defun ty-functor (typep fmap)
  "adds a functor definition to the list"
  (setq ty-functors (cons '(typep fmap) ty-functors)))



(length ty-functors)


(message ty.functors)

(defun ty.super (func)
  "returns a version of the function that can act on cool values"
