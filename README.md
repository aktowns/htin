# Socks
Socks is a toy s-expression language written in haskell. 

## Examples

### Higher order functions

```clojure
(= {adder} (\ {x} (+ x 10)))
(map adder {1 2 3 4 5}) 
; {11 12 13 14 15}
```

### Exception handling

```clojure
(catch {eval (error "ohh nooo")} (\ {err} {println (join "an error happened! " (show err))})) 
; an error happened! ohh nooo
```

### Currying

```clojure
(\ {x y} {+ x y})
(= {add} (\ {x y} {+ x y}))
(= {add-10} (add 10))
(add-10 90)
; 100
```

### FFI Example
This example uses libdiscount to convert markdown text to html. 

```clojure
(= {libmarkdown} (ffi/dlopen "/usr/lib/libmarkdown.so"))
(= {mkd-string-sym} (ffi/dlsym libmarkdown "mkd_string"))
(= {markdown-sym} (ffi/dlsym libmarkdown "markdown"))

(= {libc} (ffi/dlopen "/lib/libc.so.6"))
(= {stdout} (ffi/deref (ffi/dlsym libc "stdout")))
(= {strlen} (ffi/dlsym libc "strlen"))

(= {ffi-strlen} (ffi/call strlen ffi/sint32 {ffi/string}))
(= {ffi-mkd-string} (ffi/call mkd-string-sym ffi/ptr {ffi/string ffi/sint32 ffi/sint32}))
(= {ffi-markdown} (ffi/call markdown-sym ffi/sint32 {ffi/ptr ffi/ptr ffi/sint32}))

(fun {markdown input} "(markdown input)\nconverts and prints the given markdown string to html" { do
  (= {doc} (ffi-mkd-string input (ffi-strlen input) 0))
  (ffi-markdown doc stdout 0)})
```

