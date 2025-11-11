### 11)
| N     | Tamaño | ¿Tiene solución única? | ¿Es deducible sin backtracking? |
|:-----:|:------:|:----------------------:|:-------------------------------:|
| 0     | 2x3    |Si                        |Si                             |
| 1     | 5x5    |Si                        |Si                             |
| 2     | 5x5    |Si                        |Si                             |
| 3     | 10x10  |Si                        |No                             |
| 4     | 5x5    |Si                        |Si                             |
| 5     | 5x5    |Si                        |Si                             |
| 6     | 5x5    |Si                        |Si                             |
| 7     | 10x10  |Si                        |No                             |
| 8     | 10x10  |Si                        |No                             |
| 9     | 5x5    |Si                        |Si                             |
| 10    | 5x5    |No                        |Si                             |
| 11    | 10x10  |Si                        |No                             |
| 12    | 15x15  |Si                        |No                             |
| 13    | 11x5   |Si                        |No                             |
| 14    | 4x4    |Si                        |Si                             |


### 12)
Teniendo en cuenta nuestra definicion del predicado replicar:

```prolog
replicar(_,0, []).
replicar(X, N, [X|T]):- N>0, J is N-1, replicar(X,J,T).
```

Debido al uso de operador aritmetricas en el caso recursivo, es imposible que este predicado sea reversible en N. Esto debido a que para que estos tengan exito:

-   En el caso de `>`, tanto la expresión del lado derecho como el izquierdo se evaluan antes de realizar la comparación. Si N no esta instanciada es imposible evaluarla, por ende falla.
-   En el caso de `is`, se evalua solo el lado derecho para ver si el resultado unifica con el izquierdo. Si N no esta instanciada la expresion aritmetrica no es evaluable, pues no hay nada a que restarle 1. Por ende, esto tambien falla.

Debido a estos dos puntos, con la implementacion actual de `replicar` no es posible que N sea reversible.