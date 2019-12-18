; For vim's digraphs (in .el form) see:
;   https://raw.githubusercontent.com/JNRowe/emacs-configs/master/digraphs.el
;   This file omits characters I'd never use, and also uses my own mnemonics.

(define-prefix-command 'digraph-map)
(global-set-key (kbd "C-c d") digraph-map)

(defun register-digraphs (key digraphs &rest _description)
  "Given a list of 2-char digraphs, and a resulting (unicode) character, enable each digraph via prefix-command `digraph-map`"
  (mapc (lambda (d) (define-key digraph-map d `(lambda () (interactive) (insert ,key)))) digraphs))
  ; TODO: keep a set of all defined digraph-strings, and warn if one is repeated.

(register-digraphs "◊" '("<>" "()"))
(register-digraphs "␣" '("sp" "  "))
(register-digraphs "—" '("--" "em" "-m" "-3")) ; mdash
(register-digraphs "–" '("en" "-n" "-2")) ; ndash
(register-digraphs "°" '("dg" "^o")) ; deg
(register-digraphs "±" '("+-")) ; pm
(register-digraphs "∓" '("-+")) ; mp

(register-digraphs "Ñ" '("~N"))
(register-digraphs "ñ" '("~n"))
(register-digraphs "Ç" '("C,"))
(register-digraphs "ç" '("c," ",c"))
(register-digraphs "Ç" '(",C" "C,"))
(register-digraphs "¿" '("i?")) ; iexcl
(register-digraphs "¡" '("i!")) ; iquest
(register-digraphs "ß" '("ss" "sz"))
(register-digraphs "Ǣ" '("AE"))
(register-digraphs "ǣ" '("ae"))

(register-digraphs "¢" '("c|"))
(register-digraphs "£" '("$$" "lb"))
(register-digraphs "¥" '("Y="))
(register-digraphs "¤" '("ox"))
(register-digraphs "€" '("E="))

(register-digraphs "©" '("cO")) ; copy
(register-digraphs "®" '("rO")) ; reg
(register-digraphs "™" '("TM")) ; trade
(register-digraphs "℠" '("SM"))
(register-digraphs "℅" '("co")) ; (c/o)
(register-digraphs "№" '("N0"))
(register-digraphs "℗" '("PO"))
(register-digraphs "℞" '("Rx"))

; ligatures
(register-digraphs "ﬀ" '("ff"))
(register-digraphs "ﬁ" '("fi"))
(register-digraphs "ﬂ" '("fl"))
(register-digraphs "ﬅ" '("ft"))
(register-digraphs "ﬆ" '("st"))


; superscripts
(register-digraphs "⁰" '("^0"))
(register-digraphs "¹" '("^1"))
(register-digraphs "²" '("^2"))
(register-digraphs "³" '("^3"))
(register-digraphs "⁴" '("^4"))
(register-digraphs "⁵" '("^5"))
(register-digraphs "⁶" '("^6"))
(register-digraphs "⁷" '("^7"))
(register-digraphs "⁸" '("^8"))
(register-digraphs "⁹" '("^9"))
(register-digraphs "⁺" '("^+"))
(register-digraphs "⁻" '("^-"))
(register-digraphs "⁼" '("^="))
(register-digraphs "⁽" '("^("))
(register-digraphs "⁾" '("^)"))
(register-digraphs "ⁿ" '("^n"))
; subscripts
(register-digraphs "₀" '("_0"))
(register-digraphs "₁" '("_1"))
(register-digraphs "₂" '("_2"))
(register-digraphs "₃" '("_3"))
(register-digraphs "₄" '("_4"))
(register-digraphs "₅" '("_5"))
(register-digraphs "₆" '("_6"))
(register-digraphs "₇" '("_7"))
(register-digraphs "₈" '("_8"))
(register-digraphs "₉" '("_9"))
(register-digraphs "₊" '("_+"))
(register-digraphs "₋" '("_-"))
(register-digraphs "₌" '("_="))
(register-digraphs "₍" '("_("))
(register-digraphs "₎" '("_)"))

;fractions
(register-digraphs "¼" '("14"))
(register-digraphs "½" '("12"))
(register-digraphs "¾" '("34"))
(register-digraphs "⅓" '("13"))
(register-digraphs "⅔" '("23"))
(register-digraphs "⅕" '("15"))
(register-digraphs "⅖" '("25"))
(register-digraphs "⅗" '("35"))
(register-digraphs "⅘" '("45"))
(register-digraphs "⅙" '("16"))
(register-digraphs "⅚" '("56"))
(register-digraphs "⅛" '("18"))
(register-digraphs "⅜" '("38"))
(register-digraphs "⅝" '("58"))
(register-digraphs "⅞" '("78"))

 ;;; math-dot
(register-digraphs "·" '("md"))
(register-digraphs "×" '("mx")) ; times



; greek letters
(register-digraphs "Α" '("gA"))
(register-digraphs "Β" '("gB"))
(register-digraphs "Γ" '("gG"))
(register-digraphs "Δ" '("gD"))
(register-digraphs "Ε" '("gE"))
(register-digraphs "Ζ" '("gZ"))
(register-digraphs "Η" '("gY"))
(register-digraphs "Θ" '("gH"))
(register-digraphs "Ι" '("gI"))
(register-digraphs "Κ" '("gK"))
(register-digraphs "Λ" '("gL"))
(register-digraphs "Μ" '("gM"))
(register-digraphs "Ν" '("gN"))
(register-digraphs "Ξ" '("gC"))
(register-digraphs "Ο" '("gO"))
(register-digraphs "Π" '("gP"))
(register-digraphs "Ρ" '("gR"))
(register-digraphs "Σ" '("gS"))
(register-digraphs "Τ" '("gT"))
(register-digraphs "Υ" '("gU"))
(register-digraphs "Φ" '("gF"))
(register-digraphs "Χ" '("gX"))
(register-digraphs "Ψ" '("gQ"))
(register-digraphs "Ω" '("gW"))

(register-digraphs "α" '("ga"))
(register-digraphs "β" '("gb"))
(register-digraphs "γ" '("gg"))
(register-digraphs "δ" '("gd"))
(register-digraphs "ε" '("ge"))
(register-digraphs "ζ" '("gz"))
(register-digraphs "η" '("gy"))
(register-digraphs "θ" '("gh"))
(register-digraphs "ι" '("gi"))
(register-digraphs "κ" '("gk"))
(register-digraphs "λ" '("gl"))
(register-digraphs "μ" '("gm"))
(register-digraphs "ν" '("gn"))
(register-digraphs "ξ" '("gc"))
(register-digraphs "ο" '("go"))
(register-digraphs "π" '("gp"))
(register-digraphs "ρ" '("gr"))
(register-digraphs "σ" '("gs"))
(register-digraphs "τ" '("gt"))
(register-digraphs "υ" '("gu"))
(register-digraphs "φ" '("gf"))
(register-digraphs "χ" '("gx"))
(register-digraphs "ψ" '("gq"))
(register-digraphs "ω" '("gw"))

;;; Roman numerals (who knew?)
(register-digraphs "Ⅰ" '("1R"))
(register-digraphs "Ⅱ" '("2R"))
(register-digraphs "Ⅲ" '("3R"))
(register-digraphs "Ⅳ" '("4R"))
(register-digraphs "Ⅴ" '("5R"))
(register-digraphs "Ⅵ" '("6R"))
(register-digraphs "Ⅶ" '("7R"))
(register-digraphs "Ⅷ" '("8R"))
(register-digraphs "Ⅸ" '("9R"))
(register-digraphs "Ⅹ" '("aR"))
(register-digraphs "Ⅺ" '("bR"))
(register-digraphs "Ⅻ" '("cR"))
(register-digraphs "ⅰ" '("1r"))
(register-digraphs "ⅱ" '("2r"))
(register-digraphs "ⅲ" '("3r"))
(register-digraphs "ⅳ" '("4r"))
(register-digraphs "ⅴ" '("5r"))
(register-digraphs "ⅵ" '("6r"))
(register-digraphs "ⅶ" '("7r"))
(register-digraphs "ⅷ" '("8r"))
(register-digraphs "ⅸ" '("9r"))
(register-digraphs "ⅹ" '("ar"))
(register-digraphs "ⅺ" '("br"))
(register-digraphs "ⅻ" '("cr"))

(register-digraphs "←" '("<-"))
(register-digraphs "↑" '("^|"))
(register-digraphs "→" '("->"))
(register-digraphs "↓" '("v|"))
;(register-digraphs "↔" '())
(register-digraphs "↕" '("^v"))
(register-digraphs "⇐" '("<="))
(register-digraphs "⇒" '("=>"))
(register-digraphs "⇔" '("=="))
(register-digraphs "¬" '("-,"))
(register-digraphs "∧" '("&&"))
(register-digraphs "∧" '("AN"))
(register-digraphs "∨" '("||"))
(register-digraphs "∨" '("OR"))
(register-digraphs "∀" '("FA"))
(register-digraphs "∃" '("TE"))
(register-digraphs "∅" '("/0"))
(register-digraphs "∩" '("(U"))
(register-digraphs "∪" '(")U"))
(register-digraphs "∈" '("(-"))
(register-digraphs "∋" '("-)"))

(register-digraphs "∏" '("*P"))
(register-digraphs "∑" '("+Z"))

(register-digraphs "∆" '("DE"))
(register-digraphs "∇" '("NB"))
(register-digraphs "∂" '("dP"))
(register-digraphs "∗" '("*-")) ; lowast
(register-digraphs "∘" '("Ob" "fg")) ; for function-composition, and hollow-bullet?
(register-digraphs "∙" '("Sb"))
(register-digraphs "√" '("sq"))
(register-digraphs "∝" '("0("))
(register-digraphs "∞" '("00")) ; (infinity)
(register-digraphs "∟" '("-L"))
(register-digraphs "∠" '("-V"))
(register-digraphs "∥" '("PP")) ; (note: "||" used by LOGICAL-OR)

(register-digraphs "≤" '("<=" "le"))
(register-digraphs "≥" '(">=" "ge"))
(register-digraphs "≠" '("!=" "ne"))

(register-digraphs "⋮" '("':"))  ; TRICOLON
(register-digraphs "⋮" '(":3"))
(register-digraphs "⁞" '(":4")) ; VERTICAL FOUR DOTS
(register-digraphs "…" '(".." ".3")) ; hellip
(register-digraphs "‥" '(".2"))

(register-digraphs "′" '("''" "p'" "p1")) ; prime
(register-digraphs "″" '("p2")) ; Prime  DOUBLE PRIME
(register-digraphs "‴" '("p3")) ;        TRIPLE PRIME
(register-digraphs "⁗" '("p4")) ;        QUADRUPLE PRIME

(register-digraphs "⁇" '("??")) ; DOUBLE QUESTION MARK
(register-digraphs "⁈" '("?!")) ; QUESTION EXCLAMATION MARK
(register-digraphs "⁉" '("!?")) ; EXCLAMATION QUESTION MARK

(register-digraphs "⊥" '("-T"))
; math-<
(register-digraphs "〈" '("m<"))
(register-digraphs "〉" '("m>"))

(register-digraphs "∴" '(".:"))
(register-digraphs "∶" '(":R"))
(register-digraphs "∷" '("::"))
(register-digraphs "∼" '("~1"))
(register-digraphs "≈" '("~2"))
(register-digraphs "≈" '("~~"))
(register-digraphs "≡" '("=3"))
;(register-digraphs "≪" '("<<"))
;(register-digraphs "≫" '(">>"))
(register-digraphs "≮" '("/<"))
(register-digraphs "≯" '("/>"))
(register-digraphs "⊂" '("(C"))
(register-digraphs "⊃" '(")C"))
(register-digraphs "⊆" '("(_"))
(register-digraphs "⊇" '(")_"))
(register-digraphs "⋅" '(".P"))
(register-digraphs "⌈" '( "lc")) ; "left ceiling"
(register-digraphs "⌉" '( "rc")) ; "right ceiling"; beware "rc" used by russian
(register-digraphs "⌊" '( "lf")) ; "left floor"; "fl" a ligature
(register-digraphs "⌋" '( "rf")) ; "right floor"; beware "rf" used by russian
(register-digraphs "⌒" '("(A"))

(register-digraphs "∑" '("Su")) ; sum
(register-digraphs "∏" '("Pr")) ; prod
(register-digraphs "∫" '("In")) ; int
(register-digraphs "⌠" '("Iu"))
(register-digraphs "⌡" '("Il"))
(register-digraphs "∬" '("DI"))
(register-digraphs "∮" '("Io"))



(register-digraphs "§" '("SS"))
(register-digraphs "¶" '("pa"))  ; pilcrow
(register-digraphs "‰" '("%0")) ; PER MILLE SIGN
(register-digraphs "‱" '("%%")) ; PER TEN THOUSAND SIGN
(register-digraphs "Å" '("AO")) ; (angstrom)

(register-digraphs "†" '("+|"))
(register-digraphs "‡" '("++"))


(register-digraphs "℃" '("dC"))
(register-digraphs "℉" '("dF"))

; cards
(register-digraphs "♠" '("cS"))
(register-digraphs "♡" '("cH"))
(register-digraphs "♢" '("cD"))
(register-digraphs "♣" '("cC"))
; Music
(register-digraphs "♩" '("M4"))
(register-digraphs "♪" '("M8"))
(register-digraphs "♫" '("MF"))
(register-digraphs "♭" '("Mb"))
(register-digraphs "♮" '("Mn"))
(register-digraphs "♯" '("M#"))

(register-digraphs "✓" '("ok"))
(register-digraphs "✓" '("ck"))
(register-digraphs "✗" '("xx"))

(register-digraphs "♀" '("XX"))
(register-digraphs "♂" '("YY"))
(register-digraphs "☆" '("*0"))
(register-digraphs "★" '("*1"))
(register-digraphs "☼" '("SU"))
(register-digraphs "☺" '(":)"))
(register-digraphs "☻" '("1)"))
