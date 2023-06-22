#lang racket

(require basic-cc/language)
(require basic-cc/visitor)

(define-language juhz
  "\\s+"
  (EQ "=") (SEMICOLON ";") (DOT "\\.") (COMMA ",")
  (PLUS "\\+") (MINUS "-") (TIMES "\\*") (DIVIDE "/")
  (AND "&&") (OR "\\|\\|") (BANG "!")
  (LT "<") (GT ">") (SAME "==") (DIFF "!=") (LE "<=") (GE ">=")
  (ROUNDLEFT "\\(") (ROUNDRIGHT "\\)")
  (SQUARELEFT "\\[") (SQUARERIGHT "\\]")
  (CURLYLEFT "\\{") (CURLYRIGHT "\\}")
  (IDENT "[a-zA-Z_][a-zA-Z0-9_]*")
  (NUMBER "(?:[1-9][0-9]*)|0|(?:(?:[1-9][0-9]*)?\\.[0-9]+)")
  (STRING "\"(?:\\\\\"|\\\\n|\\\\\\\\|[^\"\\\\])*\"")
  (TRUE "true") (FALSE "false")
  (IF "if") (ELSE "else") (WHILE "while") (PACKAGE "package") (DEF "def")
  (program statement (statement program))
  (statement (expression SEMICOLON)
             (DEF left-value EQ expression SEMICOLON)
             (DEF left-value EQ CURLYLEFT program CURLYRIGHT)
             (IDENT EQ expression SEMICOLON)
             (IDENT EQ CURLYLEFT program CURLYRIGHT)
             (IF expression CURLYLEFT program CURLYRIGHT)
             (IF expression CURLYLEFT program CURLYRIGHT ELSE CURLYLEFT program CURLYRIGHT)
             (WHILE expression CURLYLEFT program CURLYRIGHT))
  (left-value IDENT (IDENT ROUNDLEFT ROUNDRIGHT) (IDENT ROUNDLEFT parameter-list ROUNDRIGHT))
  (parameter-list IDENT (IDENT COMMA parameter-list))
  (expression array package operation)
  (atom NUMBER STRING TRUE FALSE)
  (callable IDENT call indexing selection)
  (call (callable ROUNDLEFT ROUNDRIGHT) (callable ROUNDLEFT argument-list ROUNDRIGHT))
  (array (SQUARELEFT SQUARERIGHT) (SQUARELEFT argument-list SQUARERIGHT))
  (package (PACKAGE CURLYLEFT program CURLYRIGHT)
           (PACKAGE ROUNDLEFT ROUNDRIGHT CURLYLEFT program CURLYRIGHT)
           (PACKAGE ROUNDLEFT parameter-list ROUNDRIGHT CURLYLEFT program CURLYRIGHT))
  (indexing (callable SQUARELEFT expression SQUARERIGHT))
  (selection (callable DOT IDENT))
  (argument-list expression (expression COMMA argument-list))
  (operation operation#0)
  (operation#0 operation#1 (expression operator#0 operation#1))
  (operator#0 OR)
  (operation#1 operation#2 (operation#1 operator#1 operation#2))
  (operator#1 AND)
  (operation#2 operation#3 (operation#2 operator#2 operation#3))
  (operator#2 SAME DIFF)
  (operation#3 operation#4 (operation#3 operator#3 operation#4))
  (operator#3 LT GT LE GE)
  (operation#4 operation#5 (operation#4 operator#4 operation#5))
  (operator#4 PLUS MINUS)
  (operation#5 operation#6 (operation#5 operator#5 operation#6))
  (operator#5 TIMES DIVIDE)
  (operation#6 atom callable (operator#6 operation#6))
  (operator#6 BANG PLUS MINUS))

(define operation-list
  '(operation#0 operation#1 operation#2 operation#3 operation#4 operation#5 operation#6))

(define (repl tag lst)
  (cons tag (rest lst)))

(define (cons2 elt lst)
  (list* (first lst)
         elt
         (rest lst)))

(define-visitor simplify
  [($operation $expression)
   #:when (memq operation operation-list)
   (simplify expression)]
  [(operation $expression) (simplify expression)]
  [($operation $operand#0 (_ $operator) $operand#1)
   #:when (memq operation operation-list)
   (list 'operation (simplify operand#0) operator (simplify operand#1))]
  [(operation#6 (_ $operator) $operand)
   (list 'operation operator (simplify operand))]
  [(program @statement @program)
   (cons2 (simplify statement) (simplify program))]
  [(statement @expression SEMICOLON) (simplify expression)]
  [(statement DEF @left-value EQ @expression SEMICOLON)
   (list 'definition (simplify left-value) (simplify expression))]
  [(statement DEF @left-value EQ _ @program _)
   (list 'definition (simplify left-value) (simplify program))]
  [(statement @IDENT EQ @expression SEMICOLON)
   (list 'assignment IDENT (simplify expression))]
  [(statement @IDENT EQ _ @program _)
   (list 'assignment IDENT (simplify program))]
  [(left-value @IDENT) IDENT]
  [(left-value @IDENT _ _) (list 'function IDENT)]
  [(left-value @IDENT _ @parameter-list _)
   (list 'function IDENT (simplify parameter-list))]
  [(parameter-list @IDENT COMMA @parameter-list)
   (cons2 IDENT (simplify parameter-list))]
  [(statement IF $condition _ @program _)
   (list 'if (simplify condition) (simplify program))]
  [(statement IF $condition _ $true-case _ _ _ $false-case _)
   (list 'if (simplify condition) (simplify true-case) (simplify false-case))]
  [(statement WHILE $condition _ @program _)
   (list 'while (simplify condition) (simplify program))]
  [(expression $any) (simplify any)]
  [(atom $any) any]
  [(callable $any) (simplify any)]
  [(call @callable _ _) (list 'call (simplify callable))]
  [(call @callable _ @argument-list _)
   (list 'call (simplify callable) (simplify argument-list))]
  [(array _ _) (list 'array)]
  [(array _ @argument-list _) (repl 'array (simplify argument-list))]
  [(package _ _ @program _) (repl 'package (simplify program))]
  [(package _ _ _ _ @program _)
   (cons2 (list 'parameter-list) (repl 'package (simplify program)))]
  [(package _ _ @parameter-list _ _ @program _)
   (cons2 (simplify parameter-list) (repl 'package (simplify program)))]
  [(indexing @callable _ @expression _) (list 'indexing (simplify callable) (simplify expression))]
  [(selection (callable @selection) DOT @IDENT)
   (append (simplify selection) (list IDENT))]
  [(selection @callable DOT @IDENT)
   (list 'selection (simplify callable) IDENT)]
  [(argument-list @expression COMMA @argument-list)
   (cons2 (simplify expression) (simplify argument-list))]
  [($wrapper $any)
   (list wrapper (simplify any))]
  [$others others])

(define (juhz-read/AST in #:file [file "(string)"])
  (simplify (juhz-read in #:file file)))

(provide juhz-cut juhz-read juhz-read/AST (rename-out [simplify juhz-simplify]))
