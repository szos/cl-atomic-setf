# cl-atomic-setf

This package implements the macro `with-atomic-setf`, which exposes the local
macros `asetf` and `atomic-setf`, which both do the same thing. These macros
track the places being set, and when a condition is signalled but not handled
within the body of `with-atomic-setf`, it triggers a reset of all places set
with `a(tomic-)setf`. Conditions to reset upon can be specified with the keyarg
`:handle-conditions`. 

## License

LGPL

