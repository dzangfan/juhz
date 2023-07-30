#lang racket

(require basic-cc/language)

(define-language juhz
  "\\s+"
  (EQ "=") (SEMICOLON ";") (DOT "\\.") (COMMA ",") (COLON ":")
  (PLUS "\\+") (MINUS "-") (TIMES "\\*") (DIVIDE "/") (REM "%")
  (AND "&&") (OR "\\|\\|") (BANG "!")
  (LT "<") (GT ">") (SAME "==") (DIFF "!=") (LE "<=") (GE ">=")
  (ROUNDLEFT "\\(") (ROUNDRIGHT "\\)")
  (SQUARELEFT "\\[") (SQUARERIGHT "\\]")
  (CURLYLEFT "\\{") (CURLYRIGHT "\\}")
  (IDENT "\\b[a-zA-Z_][a-zA-Z0-9_]*\\b")
  (NUMBER "(?:[1-9][0-9]*)|0|(?:(?:[1-9][0-9]*)?\\.[0-9]+)")
  (STRING "\"(?:\\\\\"|\\\\n|\\\\\\\\|[^\"\\\\])*\"")
  (TRUE "true") (FALSE "false")
  (IF "\\bif\\b") (ELSE "\\belse\\b") (WHILE "\\bwhile\\b") (PACKAGE "\\bpackage\\b")
  (FUNCTION "\\bfunction\\b") (DEF "\\bdef\\b") (USE "\\buse\\b")
  (program statement (statement program))
  (statement right-value
             (USE right-value)
             (DEF left-value EQ right-value)
             (callable EQ right-value)
             (call COLON right-value))
  (left-value IDENT
              (IDENT ROUNDLEFT ROUNDRIGHT)
              (IDENT ROUNDLEFT parameter-list ROUNDRIGHT)
              (PACKAGE DOT IDENT)
              (IDENT DOT IDENT))
  (right-value (expression SEMICOLON)
               (CURLYLEFT program CURLYRIGHT)
               package
               function
               condition
               loop)
  (parameter-list IDENT (IDENT COMMA parameter-list))
  (expression package function operation condition loop)
  (atom NUMBER STRING TRUE FALSE array (ROUNDLEFT expression ROUNDRIGHT))
  (callable IDENT call indexing selection)
  (call (callable ROUNDLEFT ROUNDRIGHT) (callable ROUNDLEFT argument-list ROUNDRIGHT))
  (array (SQUARELEFT SQUARERIGHT) (SQUARELEFT argument-list SQUARERIGHT))
  (package (PACKAGE CURLYLEFT program CURLYRIGHT)
           (PACKAGE ROUNDLEFT ROUNDRIGHT CURLYLEFT program CURLYRIGHT)
           (PACKAGE ROUNDLEFT parameter-list ROUNDRIGHT CURLYLEFT program CURLYRIGHT))
  (function (FUNCTION CURLYLEFT program CURLYRIGHT)
            (FUNCTION ROUNDLEFT ROUNDRIGHT CURLYLEFT program CURLYRIGHT)
            (FUNCTION ROUNDLEFT parameter-list ROUNDRIGHT CURLYLEFT program CURLYRIGHT))
  (condition (IF expression CURLYLEFT program CURLYRIGHT ELSE CURLYLEFT program CURLYRIGHT))
  (loop (WHILE expression CURLYLEFT program CURLYRIGHT))
  (indexing (callable SQUARELEFT expression SQUARERIGHT))
  (selection (callable DOT IDENT)
             (PACKAGE DOT IDENT))
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
  (operator#5 TIMES DIVIDE REM)
  (operation#6 atom callable (operator#6 operation#6))
  (operator#6 BANG PLUS MINUS)
  #;(#:allow-conflict))

(provide juhz-read)