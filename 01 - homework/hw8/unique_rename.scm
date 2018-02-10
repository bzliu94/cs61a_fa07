; 2018-02-09

; one limitation is we use get/put for hash table

; also, we use singleton to get around not having static methods

(define TRANSLATION_PREFIX_STR "g")

; init is result if list l is empty
(define (reduceWithInit fn l init)
  (if (eq? l nil)
      init
      (reduce fn l)))

(define-class (counter init_value)
  (method (getCurrValue)
	  init_value)
  (method (setCurrValue value)
	  (set! init_value value))
  (method (increment)
	  (set! init_value (+ (ask self 'getCurrValue) 1))))

(define curr_counter (instantiate counter 1))

(define-class (entity parent_entity)
  (instance-vars
   (parameter_ID nil)
   (reference_ID nil)
   (closest_lambda_ancestor_inclusive nil)
   (closest_lambda_ancestor_exclusive nil))
  (method (getParentEntity)
	  parent_entity)
  (method (haveParentEntity)
	  (not (eq? parent_entity nil)))
  ; i is 0-indexed
  (method (isIthSubexpression i)
	  (if (eq? (ask self 'haveParentEntity) #f)
	      #f
	      (let ((parent_entity (ask self 'getParentEntity)))
		(if (eq? (ask self 'isSExpr) #f)
		    #f
		    (let ((num_subexpressions (ask parent_entity 'getNumSubexpressions)))
		      (if (< num_subexpressions (+ i 1))
			  #f
			  (eq? (ask parent_entity 'getIthSubexpression i) self)))))))
  ; i is 0-indexed
  (method (getIthSubexpression i) nil)
  (method (toString do_translated translation_prefix_str) nil)
  (method (toTranslatedString prefix_str) nil)
  (method (isPrimitive) #f)
  (method (isSExpr) #f)
  (method (toPreorderPrimitiveList) nil)
  (method (toPreorderSExprList) nil)
  (method (isDirectLambdaSExpr) #f)
  (method (isDirectLambdaParameterPrimitive) #f)
  (method (isVariableReference) #f)
  (method (isDirectLambdaKeywordPrimitive) #f)
  (method (setParameterID ID_value)
	  (set! parameter_ID ID_value))
  (method (getParameterID)
	  parameter_ID)
  (method (setReferenceID ID_value)
	  (set! reference_ID ID_value))
  (method (getReferenceID)
	  reference_ID)
  (method (getClosestLambdaAncestor include_curr_sexpr)
	  (ask self 'getClosestLambdaAncestorHelper include_curr_sexpr))
  (method (getClosestLambdaAncestorHelper include_curr_sexpr)
	  ; if we are willing to consider current node as closest lambda ancestor, check it
	  (if (and (eq? include_curr_sexpr #t)
		   (eq? (ask self 'isSExpr) #t)
		   (eq? (ask self 'isDirectLambdaSExpr) #t))
	      self
	      (if (eq? (ask self 'haveParentEntity) #t)
		  (ask (ask self 'getParentEntity) 'getClosestLambdaAncestorHelper #t)
		  nil)))
  (method (toStatus)
	  (let ((status nil))
	    (begin 
	      (cond ((eq? (ask self 'isDirectLambdaSExpr) #t)
		     (let ((have_subexpressions (> (ask self 'getNumSubexpressions) 0))
			   (subexpressions (ask self 'getSubexpressions))
			   (name nil))
		       (begin
			 (if (or (eq? have_subexpressions #f)
				 (eq? (ask (car subexpressions) 'isPrimitive) #f))
			     (set! name "have no primitive operator")
			     (begin
			       (let ((first_subexpressions (car subexpressions)))
				 (let ((primitive_operator first_subexpressions))
				   (set! name (ask primitive_operator 'getCharStr))))))
			 (set! status (list name "lambda sexpr")))))
		    ((eq? (ask self 'isDirectLambdaParameterPrimitive #t))
		     (set! status (list (ask self 'getCharStr) "parameter primitive")))
		    ((eq? (ask self 'isVariableReference) #t)
		     (set! status (list (ask self 'getCharStr) "reference primitive")))
		    ((eq? (ask self 'isDirectLambdaKeywordPrimitive #t))
		     (set! status (list (ask self 'getCharStr) "lambda keyword primitive")))
		    ((and (eq? (ask self 'isDirectLambdaSExpr) #f)
			  (eq? (ask self 'isSExpr) #t))
		     (set! status "benign sexpr")))
	      status)))
  (method (getClosestLambdaAncestorInclusive)
	  closest_lambda_ancestor_inclusive)
  (method (memoizeClosestLambdaAncestorInclusive)
	  (set! closest_lambda_ancestor_inclusive
		(ask self 'getClosestLambdaAncestor #t)))
  (method (getClosestLambdaAncestorExclusive)
	  closest_lambda_ancestor_exclusive)
  (method (memoizeClosestLambdaAncestorExclusive)
	  (set! closest_lambda_ancestor_exclusive
		(ask self 'getClosestLambdaAncestor #f)))
  (method (setPrimitiveForDirectParameterName primitive name)
	  (put self name primitive))
  (method (havePrimitiveForDirectParameterName name)
	  (not (eq? (get self name) #f)))
  (method (getPrimitiveForDirectParameterName name)
	  (get self name)))

(define-class (primitive char_str parent_entity)
  (parent (entity parent_entity))
  (method (getCharStr)
	  char_str)
  (method (toString do_translated translation_prefix_str)
	  (if (eq? do_translated #t)
	      (ask self 'toTranslatedString translation_prefix_str)
	      (ask self 'getCharStr)))
  (method (toTranslatedString prefix_str)
	  (cond ((eq? (ask self 'isDirectLambdaParameterPrimitive) #t)
		 (string-append prefix_str (number->string (ask self 'getParameterID))))
		((eq? (ask self 'isVariableReference) #t)
		 (string-append prefix_str (number->string (ask self 'getReferenceID))))
		(else (ask self 'getCharStr))))
  (method (isPrimitive) #t)
  (method (toPreorderPrimitiveList)
	  (list self))
  (method (toPreorderSExprList) nil)
  (method (isDirectLambdaParameterPrimitive)
	  (if (eq? (ask self 'haveParentEntity) #f)
	      #f
	      (let ((is_lambda_keyword_primitive (string=? (ask self 'getCharStr) "lambda")))
		(let ((is_parameter_primitive
		       (and (eq? is_lambda_keyword_primitive #f)
			    (eq? (ask (ask (ask self 'getParentEntity) 'getParentEntity)
				      'isDirectLambdaSExpr) #t)
			    (eq? (ask (ask self 'getParentEntity) 'isIthSubexpression 1) #t))))
		  is_parameter_primitive))))
  (method (isVariableReference)
	  (and (eq? (ask self 'isDirectLambdaSExpr) #f)
	       (eq? (ask self 'isDirectLambdaParameterPrimitive) #f)
	       (eq? (ask self 'isDirectLambdaKeywordPrimitive) #f)))
  (method (isDirectLambdaKeywordPrimitive)
	  (string=? (ask self 'getCharStr) "lambda")))

(define-class (sexpr subexpressions parent_entity)
  (parent (entity parent_entity))
  (method (getSubexpressions)
	  subexpressions)
  (method (getNumSubexpressions)
	  (length (ask self 'getSubexpressions)))
  (method (setSubexpressions given_subexpressions)
	  (set! subexpressions given_subexpressions))
  ; i is 0-indexed
  (method (getIthSubexpression i)
	  (if (< (ask self 'getNumSubexpressions) (+ i 1))
	      (error "not enough subexpressions")
	      (list-ref (ask self 'getSubexpressions) i)))
  (method (toString do_translated translation_prefix_str)
	  (let ((core_str nil))
	    (begin
	      (if (eq? (ask self 'getNumSubexpressions) 0)
		  (set! core_str "")
		  (set! core_str
			(reduceWithInit
			 (lambda (x y)
			   (string-append x " " y))
			 (map (lambda (x)
				(ask x 'toString
				     do_translated
				     translation_prefix_str))
			      (ask self 'getSubexpressions))
			 "")))
	      (string-append "(" core_str ")"))))
  (method (toTranslatedString prefix_str)
	  (ask self 'toString #t prefix_str))
  (method (havePrimitiveOperator)
	  (if (> (ask self 'getNumSubexpressions) 0)
	      (let ((operator (car (ask self 'getSubexpressions))))
		(if (eq? (ask operator 'isPrimitive) #t)
		    #t
		    #f))
	      #f))
  (method (getPrimitiveOperator)
	  (car (ask self 'getSubexpressions)))
  (method (isSExpr) #t)
  (method (isDirectLambdaSExpr)
	  (let ((have_primitive_operator (ask self 'havePrimitiveOperator)))
	    (if (eq? have_primitive_operator #t)
		(let ((primitive_operator (ask self 'getPrimitiveOperator)))
		  ; changed the following in keeping with DRY
		  (if (eq? (ask primitive_operator 'isDirectLambdaKeywordPrimitive) #t)
		      #t
		      #f))
		#f)))
  (method (getParameterPrimitivesForDirectLambdaSExpr)
	  (if (eq? (ask self 'isDirectLambdaSExpr) #f)
	      (error "this is not a lambda sexpr")
	      (let ((subexpressions (ask self 'getSubexpressions)))
		(let ((parameter_sexpr (cadr subexpressions)))
		  (let ((parameter_primitives (ask parameter_sexpr 'getSubexpressions)))
		    parameter_primitives)))))
  (method (toPreorderPrimitiveList)
	  (let ((primitive_list
		 (reduceWithInit append
				 (map (lambda (x)
					(ask x 'toPreorderPrimitiveList))
				      (ask self 'getSubexpressions))
				 nil)))
	    primitive_list))
  (method (toPreorderSExprList)
	  (let ((sexpr_list
		 (append
		  (list self)
		  (reduceWithInit append
				  (map (lambda (x)
					 (ask x 'toPreorderSExprList))
				       (ask self 'getSubexpressions))
				  nil))))
	    sexpr_list)))

; since we have no support for static methods, we will use a singleton

(define-class (sexprSingleton)
  (method (constructUsingNestedTokenLists nested_token_lists)
	  (ask self 'constructUsingNestedTokenListsHelper nested_token_lists nil))
  (method (constructUsingNestedTokenListsHelper nested_token_lists parent_entity)
	  (if (eq? (list? nested_token_lists) #f)
	      ; remember that a quoted s-expression provides symbols, not strings
	      (let ((char_str (symbol->string nested_token_lists)))
		(instantiate primitive char_str parent_entity))
	      (let ((curr_sexpr (instantiate sexpr nil parent_entity)))
		(let ((subexpressions (map (lambda (x)
					     (ask self 'constructUsingNestedTokenListsHelper
						  x curr_sexpr))
					   nested_token_lists)))
		  (begin
		    (ask curr_sexpr 'setSubexpressions subexpressions)
		    curr_sexpr)))))
  (method (preorderNumberDirectLambdaParameterPrimitives curr_entity start_ID_value)
	  (let ((curr_counter (instantiate counter start_ID_value)))
	    (ask self 'preorderNumberDirectLambdaParameterPrimitivesHelper curr_entity curr_counter)))
  (method (preorderNumberDirectLambdaParameterPrimitivesHelper curr_entity curr_counter)
	  (cond ((eq? (ask curr_entity 'isPrimitive) #t)
		 (if (eq? (ask curr_entity 'isDirectLambdaParameterPrimitive) #t)
		     (begin 
		       (ask curr_entity 'setParameterID (ask curr_counter 'getCurrValue))
		       (ask curr_counter 'increment))
		     nil))
		((eq? (ask curr_entity 'isSexpr) #t)
		 (for-each (lambda (x)
			     (ask self 'preorderNumberDirectLambdaParameterPrimitivesHelper
				  x curr_counter))
			   (ask curr_entity 'getSubexpressions)))))
  (method (memoizeClosestLambdaAncestorsForAllEntities curr_entity)
	  (ask self 'memoizeClosestLambdaAncestorsForAllEntitiesHelper curr_entity))
  (method (memoizeClosestLambdaAncestorsForAllEntitiesHelper curr_entity)
	  (begin 
	    (ask curr_entity 'memoizeClosestLambdaAncestorInclusive)
	    (ask curr_entity 'memoizeClosestLambdaAncestorExclusive)
	    (if (eq? (ask curr_entity 'isSExpr) #t)
		(for-each (lambda (x)
			    (ask self 'memoizeClosestLambdaAncestorsForAllEntitiesHelper x))
			  (ask curr_entity 'getSubexpressions))
		nil)))
  (method (memoizeForLambdasDirectParameterNameToPrimitive curr_entity)
	  (ask self 'memoizeForLambdasDirectParameterNameToPrimitiveHelper curr_entity))
  (method (memoizeForLambdasDirectParameterNameToPrimitiveHelper curr_entity)
	  (begin
	    (if (eq? (ask curr_entity 'isDirectLambdaSExpr) #t)
		(let ((parameter_primitive_list
		       (ask curr_entity 'getParameterPrimitivesForDirectLambdaSExpr)))
		  (for-each (lambda (x)
			      (let ((name (ask x 'getCharStr)))
				(ask curr_entity 'setPrimitiveForDirectParameterName
				     x name)))
			    parameter_primitive_list))
		nil)
	    (if (eq? (ask curr_entity 'isSExpr) #t)
		(for-each (lambda (x)
			    (ask self 'memoizeForLambdasDirectParameterNameToPrimitiveHelper x))
			  (ask curr_entity 'getSubexpressions))
		nil)))
  (method (memoizeReferenceIDsForReferencePrimitives curr_entity)
	  (ask self 'memoizeReferenceIDsForReferencePrimitivesHelper curr_entity))
  (method (memoizeReferenceIDsForReferencePrimitivesHelper curr_entity)
	  (begin
	    (if (eq? (ask curr_entity 'isVariableReference) #t)
		(ask self 'memoizeReferenceIDForReferencePrimitive curr_entity)
		nil)
	    (if (eq? (ask curr_entity 'isSExpr) #t)
		(for-each (lambda (x)
			    (ask self 'memoizeReferenceIDsForReferencePrimitivesHelper x))
			  (ask curr_entity 'getSubexpressions))
		nil)))
  (method (memoizeReferenceIDForReferencePrimitive reference_primitive)
	  (let ((closest_lambda_sexpr (ask reference_primitive 'getClosestLambdaAncestorInclusive)))
	    (ask self 'memoizeReferenceIDForReferencePrimitiveHelper
		 reference_primitive closest_lambda_sexpr)))
  (method (memoizeReferenceIDForReferencePrimitiveHelper reference_primitive lambda_sexpr)
	  (if (eq? lambda_sexpr nil)
	      (error "never found a suitable lambda s.t. parameter exists under desired name")
	      (let ((name (ask reference_primitive 'getCharStr)))
		(let ((have_match (ask lambda_sexpr 'havePrimitiveForDirectParameterName name)))
		  (if (eq? have_match #t)
		      ; test whether desired name exists as a parameter for this lambda
		      (let ((parameter_primitive
			     (ask lambda_sexpr 'getPrimitiveForDirectParameterName name)))
			(ask reference_primitive 'setReferenceID
			     (ask parameter_primitive 'getParameterID)))
		      ; otherwise, continue to next-closest lambda
		      (let ((next_lambda_sexpr
			     (ask lambda_sexpr 'getClosestLambdaAncestorExclusive)))
			(ask self 'memoizeReferenceIDForReferencePrimitiveHelper
			     reference_primitive next_lambda_sexpr))))))))

; (define-class (entity parent_entity)
;   (parent)
;   (instance-vars)
;   (class-vars)
;   (default-method
;   (initialize)
;   (method))

(define line '(lambda (x) (lambda (y) (x (lambda (x) (y x))))))

; (define line '(lambda (x z) (lambda (y) (x (lambda (x) (y x))))))
; (define line '(lambda (x z) ((lambda (y) y) x)))
; (define line '(lambda () ()))
; (define line '(lambda () (lambda (x) x)))

(define curr_sexpr_singleton (instantiate sexprSingleton))
(define root_sexpr (ask curr_sexpr_singleton 'constructUsingNestedTokenLists line))
(ask curr_sexpr_singleton 'preorderNumberDirectLambdaParameterPrimitives root_sexpr 1)
(ask curr_sexpr_singleton 'memoizeClosestLambdaAncestorsForAllEntities root_sexpr)
(ask curr_sexpr_singleton 'memoizeForLambdasDirectParameterNameToPrimitive root_sexpr)
(ask curr_sexpr_singleton 'memoizeReferenceIDsForReferencePrimitives root_sexpr)
(display (ask root_sexpr 'toTranslatedString TRANSLATION_PREFIX_STR))


