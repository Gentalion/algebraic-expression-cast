;Pavel Yeltsin, gr.324, 16.12.2018, version 0.4

(defparameter charbuf NIL)

(defun main ()
	(setq input (readRawData T))

	(format t "~A" #\<)
	(printRawData (excludePlusMinusTokens (divideInComplexTokens(rewriteDegrees input NIL) NIL)))
	(format t "~A~%~%" #\>)

	(format t "~A" #\<)
	(printRawData (produceAllMultiplies (excludePlusMinusTokens (divideInComplexTokens(rewriteDegrees input NIL) NIL)) NIL))
	(format t "~A~%~%" #\>)


	(format t "~A" #\<)
	(printRawData (castSumAll (unifyAllTokens (removeExtraBrackets (produceAllMultiplies (excludePlusMinusTokens (divideInComplexTokens(rewriteDegrees input NIL) NIL)) NIL))) NIL))
	(format t "~A~%~%" #\>)

	(printCastedData (castSomeSpells input) T)

	;(produceAllMultiplies (excludePlusMinusTokens (divideInComplexTokens(rewriteDegrees input NIL) NIL)) NIL)

	(ext:exit)
)

(defun printRawData (data)
	(cond
		((null data) NIL)
		((atom (car data)) (format t "~A" (car data)) (printRawData (cdr data)))
		(T (format t "~A" #\<) (printRawData (car data)) (format t "~A" #\>) (printRawData (cdr data)))
	)
)

(defun printCastedData (data isFirst)
	(cond
		((null data) NIL)
		((or (eq isFirst T) (< (cadar data) 0)) (format t "~A" (cadar data)) (printTokenName (caar data)) (printCastedData (cdr data) NIL))
		(T (format t "+~A" (cadar data)) (printTokenName (caar data)) (printCastedData (cdr data) NIL))
	)
)

(defun printTokenName (data)
	(cond 
		((null data) NIL)
		((numberp (car data)) (format t "^~A" (car data)) (printTokenName (cdr data)))
		(T (format t "~A" (car data)) (printTokenName (cdr data)))
	)
)

(defun invert (l acc)
	(cond
		((null l) acc)
		(T (invert (cdr l) (cons (car l) acc)))
	)
)

(defun castSomeSpells (rawData)
	(castSumAll (unifyAllTokens (removeExtraBrackets (produceAllMultiplies (excludePlusMinusTokens (divideInComplexTokens (rewriteDegrees input NIL) NIL)) NIL))) NIL)
)

(defun castSumAll (data acc)
	(cond
		((null data) (invert acc NIL))
		(T (castSumAll (cdr data) (castAddOne (car data) acc)))
	)
)

(defun castAddOne (new casted)
	(cond
		((null casted) (cons new NIL))
		((equal (car new) (caar casted)) (cons (list (car new) (+ (cadr new) (cadar casted))) (cdr casted)))
		(T (cons (car casted) (castAddOne new (cdr casted))))
	)
)

(defun unifyAllTokens (data)
	(cond
		((null data) NIL)
		(T (cons (unifyToken (car data)) (unifyAllTokens (cdr data))))
	)
)

(defun unifyToken (token)
	(cond
		((null token) (print "error") NIL)
		(T (cons (charSort (car token) NIL) (cdr token)))
	)
)

(defun charSort (data acc)
	(cond
		((null data) acc)
		(T (charSort (cdr data) (charSortOnePlusMakeDegree (car data) acc)))
	)
)

(defun charSortOnePlusMakeDegree (new sorted)
	(cond
		((null sorted) (cons new NIL))
		((not (characterp (car sorted))) (cons (car sorted) (charSortOnePlusMakeDegree new (cdr sorted))))
		((char= new (car sorted))
			(cond
				((null (cdr sorted)) (list new 2))
				((characterp (cadr sorted)) (cons new (cons 2 (cdr sorted))))
				(T (cons new (cons (+ (cadr sorted) 1) (cddr sorted))))
			)
		)
		((char> new (car sorted)) (cons (car sorted) (charSortOnePlusMakeDegree new (cdr sorted))))
		(T (cons new sorted))
	)
)

(defun removeExtraBrackets (data)
	(cond
		((null data) NIL)
		((listp (caaar data)) (append (removeExtraBrackets (caar data)) (removeExtraBrackets (cdr data))))
		(T (cons (car data) (removeExtraBrackets (cdr data))))
	)
)

(defun excludePlusMinusTokens (data)
	(excludePlusMinusTokensPhysically (excludePlusMinusTokensLogically data) NIL)
)

(defun excludePlusMinusTokensPhysically (data acc)
	;(format t "<")
	;(printRawData data) (format t ">~%")
	(cond
		((null data) (invert acc NIL))
		((or (eq (caar data) #\+) (eq (caar data) #\-)) (excludePlusMinusTokensPhysically (deleteMultiplySignFromTop (cdr data)) (deleteMultiplySignFromTop acc)))
		((isOperand (caar data)) (excludePlusMinusTokensPhysically (cdr data) (cons (car data) acc)))
		((numberp (caar data)) (excludePlusMinusTokensPhysically (cdr data) (cons (car data) acc)))
		((and (listp (caaar data)) (not (null (caaar data)))) (excludePlusMinusTokensPhysically (cdr data) (cons (cons (excludePlusMinusTokensPhysically (caar data) NIL) (cdar data)) acc)))
		(T (excludePlusMinusTokensPhysically (cdr data) (cons (car data) acc)))
	)
)

(defun deleteMultiplySignFromTop (data)
	(cond
		((null data) NIL)
		((eq (caar data) #\*) (cdr data))
		(T data)
	)
)

(defun excludePlusMinusTokensLogically (data)
	(cond
		((null data) NIL)
		((eq (caar data) #\+) (cons (car data) (excludePlusMinusTokensLogically (cdr data))))
		((eq (caar data) #\-) 
			(cond
				((eq (caadr data) #\-) (cons (car data) (excludePlusMinusTokensLogically (cddr data))))
				((eq (caadr data) #\+) (cons (car data) (excludePlusMinusTokensLogically (cons (list #\-) (cddr data)))))
				(T (cons (car data) (cons (cons (caadr data) (list (* (cadadr data) (- 1)))) (excludePlusMinusTokensLogically (cddr data)))))
			)
		)
		((eq (caar data) #\*) (cons (car data) (excludePlusMinusTokensLogically (cdr data))))
		((listp (caaar data)) (cons (list #\*) (cons (cons (excludePlusMinusTokensLogically (caar data)) (cdar data)) (cons (list #\*) (excludePlusMinusTokensLogically (cdr data))))))
		(T (cons (car data) (excludePlusMinusTokensLogically (cdr data))))
	)
)

(defun produceAllMultiplies (data acc)
	(format t "<") (printRawData acc) (format t ">~%")
	(cond
		((null data) (invert acc NIL))
		((null acc)
			(if (listp (caar data))
				(if (listp (caaar data))
					(produceAllMultiplies (cdr data) (list (cons (produceAllMultiplies (caar data) NIL) (cdar data))))
					(produceAllMultiplies (cdr data) (cons (car data) acc))
				)
				(produceAllMultiplies (cdr data) acc)
			)
		)
		((eq (caar data) #\*)
			(if (and (not (null (cdr data))) (not (null acc)))
				(if (eq (caadr data) #\*)
					(produceAllMultiplies (cdr data) acc)
					(if (null (caadr data))
						(produceAllMultiplies (cddr data) (cons (list (caar acc) (* (cadar acc) (cadadr data))) (cdr acc)))
						(if (listp (caaadr data))
							(produceAllMultiplies (cddr data) (produceOneMultiply (cons (produceAllMultiplies (caadr data) NIL) (cdadr data)) (car acc) (cdr acc) T))
							(produceAllMultiplies (cddr data) (produceOneMultiply (cadr data) (car acc) (cdr acc) T))
						)
					)
				)
				(produceAllMultiplies (cdr data) acc)
			)
		)
		((isOperand (caar data)) (produceAllMultiplies (cdr data) (cons (car data) acc)))
		;((listp (caaar data)) (produceAllMultiplies (cdr data) (produceOneMultiply (cons (produceAllMultiplies (caar data) NIL) (cdar data)) (car acc) (cdr acc))))
		(T  (produceAllMultiplies (cdr data) (cons (car data) acc)))
	)
)

(defun produceOneMultiply (op1 op2 acc isFirst)
	(print op1) (print op2) (print acc)
	(cond
		((or (null (caar op1)) (null (caar op2))) acc)
		((and (atom (caar op1)) (atom (caar op2)))
			(addToAcc (list (append (car op1) (car op2)) (* (cadr op1) (cadr op2))) acc)
		)
		((and (atom (caar op1)) (not (atom (caar op2))))
			(if (and (not (null acc)) (eq isFirst NIL))
				(produceOneMultiply op1 (cons (cdar op2) (cdr op2)) (produceOneMultiply op1 (caar op2) acc NIL) NIL)
				(produceOneMultiply op1 op2 (cons (list (list NIL) 1) acc) NIL)
			)
		)
		((and (not (atom (caar op1))) (atom (caar op2)))
			(produceOneMultiply op2 op1 acc T)
		)
		(T ; перемножение двух скобок 
			(if (and (not (null acc)) (eq isFirst NIL)) 
				(produceOneMultiply (cons (cdar op1) (cdr op1)) op2 (produceOneMultiply (caar op1) op2 acc NIL) NIL)
				(produceOneMultiply op1 op2 (cons (list (list NIL) 1) acc) NIL)
			)
		)
	)
)

(defun addToAcc (new acc)
	(cond
		((null acc) (cons new acc))
		((listp (caaar acc)) 
			(if (null (caaar acc))
				(cons (cons (list new) (cdar acc)) (cdr acc))
				(cons (cons (cons new (caar acc)) (cdar acc)) (cdr acc))
			)
		)
		(T (cons new acc))
	)
)

(defun rewriteDegrees (rawData acc)
	(cond
		((null rawData) (invert acc NIL))
		((eq (car rawData) #\^)
			(cond
				((null (cdr rawData)) (print "error 1") NIL)
				((not (numberp (cadr rawData))) (print "error 2") NIL)
				((eq (cadr rawData) 1) (rewriteDegrees (cddr rawData) acc))
				(T (rewriteDegrees (cons (car rawData) (cons (- (cadr rawData) 1) (cddr rawData))) (cons (car acc) acc) ))
			)
		)
		(T (rewriteDegrees (cdr rawData) (cons (car rawData) acc)))
	)
)

(defun divideInComplexTokens (rawData acc)
	(cond
		((null rawData) (invert acc NIL))
		(T 
			(let ((separate (separateComplexToken rawData NIL)))
				(if (listp (caar separate))
					(divideInComplexTokens (cdr separate) (cons (makeComplexToken (divideInComplexTokens (caar separate) NIL) NIL) acc))
					(divideInComplexTokens (cdr separate) (cons (makeComplexToken (car separate) NIL) acc))
				)
			)
		)
	)
)

(defun separateComplexToken (l acc)
	(cond
		((null l) (cons (invert acc NIL) l))
		((not (atom (car l))) 
			(if (null acc) 
				(cons (cons (car l) NIL) (cdr l))
				(cons (invert acc NIL) l)
			)
		)
		((isOperand (car l)) 
			(if (null acc) 
				(cons (cons (car l) NIL) (cdr l))
				(cons (invert acc NIL) l)
			)
		)
		(T (separateComplexToken (cdr l) (cons (car l) acc)))
	)
)

(defun makeComplexToken (rawComplexToken acc)
	(cond
		((null rawComplexToken) acc)
		((null acc) 
			(if (isOperand (car rawComplexToken))
				(cons (car rawComplexToken) NIL)
				(if (numberp (car rawComplexToken))
					(makeComplexToken (cdr rawComplexToken) (list NIL (car rawComplexToken)))
					(makeComplexToken (cdr rawComplexToken) (list (list (car rawComplexToken)) 1))
				)
			)
		)
		((numberp (car rawComplexToken)) (makeComplexToken (cdr rawComplexToken) (list (car acc) (* (cadr acc) (car rawComplexToken)))))
		(T (makeComplexToken (cdr rawComplexToken) (cons (cons (car rawComplexToken) (car acc)) (cdr acc))))
	)
)

(defun readRawData (isFirst)
	(let ((curToken (readAnyToken isFirst)))
		(cond
			((null curToken) NIL)
			((numberp curToken) (cons curToken (readRawData NIL)))
			(T (cons curToken (readRawData T)))
		)
	)
)

(defun readAnyToken (isFirst)
	(if (eq isFirst T)
		(defparameter charbuf (read-char *standard-input* NIL NIL))
		NIL
	)
	(cond
		((null charbuf) NIL)
		((alpha-char-p charbuf) charbuf) ; однобуквенная переменная
		((isOperand charbuf) charbuf) ; +,-,* и ^
		((digit-char-p charbuf) (parse-integer (coerce (readNumberToken T) 'string)))
		((eq charbuf #\() (readRawData T))
		((eq charbuf #\)) NIL)
		((eq charbuf #\Space) (readAnyToken T))
		(T (print "error?"))
	)
)

(defun isOperand (symbol)
	(cond
		((eq symbol #\+) T)
		((eq symbol #\-) T)
		((eq symbol #\*) T)
		((eq symbol #\^) T)
		(T NIL)
	)
)

(defun readNumberToken (isFirst)
	(if (eq isFirst T)
		(cons charbuf (readNumberToken NIL))
		(let ((cur (read-char *standard-input* NIL NIL)))
			(cond
				((null cur) (defparameter charbuf cur) NIL)
				((digit-char-p cur) (cons cur (readNumberToken NIL)))
				(T (defparameter charbuf cur) NIL)
			)
		)
	)
)

(ext:saveinitmem "AEConLisp" :init-function 'main :executable t :quiet t)