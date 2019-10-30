# Notes on the Little Schemer

```scheme
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
```

> __The Law of Car___
>
> The primitive ___car___ is defined only for non-empty lists

> __The Law of Cdr__
>
> The primitive ___cdr___ is defined only for non-empty lists. The ___cdr___ of any non-empty list is always another list.

> __The Law of Cons__
>
> The primitive ___cons___ takes two arguments. The second argument to ___cons___ must be a list. The result is a list.

> __The Law of Null?__
>
> The primitive ___null?___ is defined only of lists.

> __The Law of Eq?__
>
> The primitive ___eq?___ takes two arguments. Each must be a non-numeric atom.

```scheme
(define lat?
  (lambda (l)
    (cond
     ((null? l) #t)
     ((atom? (car l)) (lat? (cdr l)))
     (else #f))))
```

```scheme
(define member?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else (or (eq? (car lat) a)
               (member? a (cdr lat)))))))
```

> __The First Commandment__
>
> (___preliminary___)
>
> Always ask ___null?___ as the first question in expressing any function.

>__The Second Commandment__
>
>Use ___cons___ to build lists.

```scheme
(define rember
  (lambda (a lat)
    (cond
     ((null? lat) '())
     (else (cond
            ((eq? (car lat) a) (cdr lat))
            ((else (cons (car lat)
                         (rember a (cdr lat))))))))))
```

__Rewrite _rember_ function__

```scheme
(define rember
  (lambda (a lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) a) (cdr lat))
     (else (cons (car lat) rember a (cdr lat))))))
```

> __The Third Commandment__
>
> When building a list, describe the first typical element, and then ___cons___ it onto the natural recursion.

> __The Fourth Commandment__
>
> _(preliminiary)_
>
> Always change at least on argument while recurring. It must be changed to be closer to termination. The changing argument must be tested in the termination condition: when using ___cdr___, test termination with ___null?___.

> __The First Commandment__
>
> _(first revision)_
>
> When recurring on a list of atoms, ___lat___, ask two questions about it: ___(null? lat)___ and ___else___.
>
> When recurring on a number, ___n___, ask two questions about it: ___(zero? n)___ and ___else___.

> __The Fourth Commandment__
>
> _(first revision)_
>
> Always change at least one argument while recurring. It must be changed to be closer to termination. The changing argument must be tested in the terminating condition:
>
> when using ___cdr___, test termination with ___null?___ and
>
> when using ___sub1___, test termination with ___zero?___.

> __The Fifth Commandment__
>
> When building a value with ___+___, always use ___0___ for the value of the terminating line, for adding ___0___ does not change the value of addition.
>
> When building a value with ___x___, always use ___1___ for the value of the terminating line, for multiplying by ___1___ does not change the value of a multiplication.
>
> When building a value with ___cons___, always consider ___()___ for the value of the terminating line.

> __The First Commandment__
>
> _(final version)_
>
> When recurring on a list of atoms, ___lat___, ask two questions about it: ___(null? lat)___ and ___else___.
>
> When recurring on a number, ___n___, ask two questions about it: ___(zero? n)___ and ___else___.
>
> When recurring on a list of S-expressions, ___l___, ask three questions about it: ___(null? l)___, ___(atom? (car l))___, and ___else___.

> __The Fourth Commandment__
>
> _(final version)_
>
> Always change at least one argument while recurring. When recurring on a list of atoms, ___lat___, use ___(cdr lat)___. When recurring on a number, ___n___, use ___(sub1 n)___. And when recurring on a list of S-expressions, ___l___, use ___(car l)___ and ___(cdr l)___ if neither ___(null? l)___ nor ___(atom ? (car l))___ are true.
>
> It must be changed to be close to termination. The changing argument must be tested in the termination condition.
>
> - when using ___cdr___, test termination with ___null?___ and
> - when using ___sub1___, test termination with ___zero?___.

> __The Sixth Commandment__
>
> Simplify only after the function is correct.

> __The Seventh Commandment__
>
> Recur on the the ___subparts___ that are of the same nature:
>
> - On the sublists of a list.
> - On the subexpressions of an arithmetic expression

> __The Eighth Commandment__
>
> Use help functions to abstract from representations.

> __The Ninth Commandment__
>
> Abstract common patterns with a new function.

> __The Tenth Commandment__
>
> Build functions to collect more than one value at a time.

