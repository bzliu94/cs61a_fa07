; 2018-01-22

; finished translating from python

; 2018-01-19

; key is difference between explicit and implicit type;
; also, we have type consolidation as before

; we are able to use hash tables via put and get

; we don't use put/get for packages or generic operators
; as they were introduced in sections 2.4 through 2.5.2 for

; also, we don't take advice from the professor
; that the really ambitious should use a table (and therefore put/get)
; to store current variable type status; by a re-arranging, 
; we do essentially the same thing except as a separate phase
; (called consolidation) at the end

; we have nodes that have parent and child pointers via hash table

; extra credit

; type inference

(define UNKNOWN_CODE 0)
(define PROCEDURE_CODE 1)
(define NUMBER_CODE 2)
(define LIST_CODE 3)
(define SENTENCE_OR_WORD_CODE 4)
(define CONFLICTING_CODE 5)

(define (getTypeStr typeCode)
  (cond ((eq? typeCode UNKNOWN_CODE) '?)
	((eq? typeCode PROCEDURE_CODE) 'procedure)
	((eq? typeCode NUMBER_CODE) 'number)
	((eq? typeCode LIST_CODE) 'list)
	((eq? typeCode SENTENCE_OR_WORD_CODE) 'sentence-or-word)
	((eq? typeCode CONFLICTING_CODE) 'x)))

; node-related logic

(define nodeIDCounter 0)

;; first part of constructor

(define (createNode name isApplication)
  (let ((currNodeID nodeIDCounter))
    (set! nodeIDCounter (+ nodeIDCounter 1))
    (list 'node currNodeID name isApplication)))

;; non-existent children -> nil
;; non-existent parent -> nil

;; second part of constructor; many methods
;; for a node will not work without this being called

(define (postProcessNode node childrenIDs parentID)
  (setNodeChildrenIDs node childrenIDs)
  (setNodeParentID node parentID)
  (setNodeUsingID node)
  (setNodeTypeCode node UNKNOWN_CODE))

; bare node getters

(define (getNodeID node)
  (car (cdr node)))

(define (getNodeName node)
  (car (cdr (cdr node))))

(define (getNodeIsApplication node)
  (car (cdr (cdr (cdr node)))))

; non-bare node getters/setters

(define (setNodeChildrenIDs node childrenIDs)
  (let ((currNodeID (getNodeID node)))
    (put 'nodeChildren currNodeID childrenIDs)))

(define (setNodeParentID node parentID)
  (let ((currNodeID (getNodeID node)))
    (put 'nodeParent currNodeID parentID)))

;; aliasing occurs

(define (setNodeUsingID node)
  (let ((currNodeID (getNodeID node)))
    (put 'nodeUsingID currNodeID node)))

(define (setNodeTypeCode node typeCode)
  (let ((currNodeID (getNodeID node)))
    (put 'nodeTypeCode currNodeID typeCode)))

(define (getNodeChildrenIDs node)
  (let ((currNodeID (getNodeID node)))
    (get 'nodeChildren currNodeID)))

(define (getNodeParentID node)
  (let ((currNodeID (getNodeID node)))
    (get 'nodeParent currNodeID)))

(define (getNodeTypeCode node)
  (let ((currNodeID (getNodeID node)))
    (get 'nodeTypeCode currNodeID)))

(define (haveChildren node)
  (not (eq? (getNodeChildrenIDs node) nil)))

(define (haveParent node)
  (not (eq? (getNodeParentID node) nil)))

;; aliasing occurs

(define (getNodeUsingID nodeID)
  (get 'nodeUsingID nodeID))

; auxiliary methods

(define (getNodesUsingIDs nodeIDs)
  (map (lambda (x) (getNodeUsingID x)) nodeIDs))

(define (getChildren node)
  (let ((childrenIDs (getNodeChildrenIDs node)))
    (getNodesUsingIDs childrenIDs)))

(define (getNumChildren node)
  (length (getChildren node)))

(define (getParent node)
  (if (eq? (haveParent node) #t)
      (let ((parentID (getNodeParentID node)))
	(getNodeUsingID parentID))
      nil))

(define (setParent node parent)
  (let ((parentID (getNodeID parent)))
    (setNodeParentID node parentID)))

; higher-level methods

(define (toFlattenedList node)
  (let ((currChildren (getChildren node))
	(numChildren (getNumChildren node)))
    (if (eq? numChildren 0)
	(list node)
	(append (list node)
		(reduce (lambda (x y) (append x y))
			(map (lambda (x) (toFlattenedList x))
			     currChildren))))))

(define (isFirstChild node)
  (if (eq? (haveParent node) #f)
      #f
      (let ((currParent (getParent node)))
	(if (not (>= (getNumChildren currParent) 1))
	    #f
	    (let ((parentChildren (getChildren currParent)))
	      (let ((firstChild (car parentChildren)))
		(if (eq? (getNodeID firstChild)
			 (getNodeID node))
		    #t
		    #f)))))))

(define (isSecondChild node)
  (if (eq? (haveParent node) #f)
      #f
      (let ((currParent (getParent node)))
	(if (not (>= (getNumChildren currParent) 2))
	    #f
	    (let ((parentChildren (getChildren currParent)))
	      (let ((secondChild (car (cdr parentChildren))))
		(if (eq? (getNodeID secondChild)
			 (getNodeID node))
		    #t
		    #f)))))))

(define (checkParentForName node name)
  (if (eq? (haveParent node) #f)
      #f
      (let ((currParent (getParent node)))
	(if (eq? (getNodeName currParent) name)
	    #t
	    #f))))

(define (checkParentsForName node name)
  (if (eq? (haveParent node) #f)
      #f
      (let ((currParent (getParent node)))
	(if (eq? (getNodeName currParent) name)
	    #t
	    (checkParentsForName currParent name)))))

;; first provided type

(define (checkIsProcedure node)
  (let ((currParent (getParent node))
	(currHaveParent (haveParent node)))
    (let ((currParentName (if (eq? currHaveParent #t)
			      (getNodeName currParent)
			      nil)))
      (cond ((and
	     (eq? (checkParentsForName node 'quote) #f)
	     (eq? (getNodeIsApplication node) #t))
	     #t)
	    ((and
	      (eq? (isFirstChild node) #t)
	      (or
	       (eq? currParentName 'map)
	       (eq? currParentName 'every))
	      (eq? (getNodeIsApplication node) #f))
	     #t)
	    (else #f)))))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

;; second provided type

(define (checkIsNumber node)
  (let ((currParent (getParent node))
	(currHaveParent (haveParent node)))
    (let ((currParentName (if (eq? currHaveParent #t)
			      (getNodeName currParent)
			      nil)))
      (cond ((and
	      (eq? (element-of-set? currParentName (list '+ '- 'max 'min)) #t)
	      (eq? (getNodeIsApplication node) #f))
	     #t)
	    (else #f)))))

;; third provided type

(define (checkIsList node)
  (let ((currParent (getParent node))
	(currHaveParent (haveParent node)))
    (let ((currParentName (if (eq? currHaveParent #t)
			      (getNodeName currParent)
			      nil)))
      (cond ((and
	      (eq? currParentName 'append)
	      (eq? (getNodeIsApplication node) #f))
	     #t)
	    ((and
	      (eq? (element-of-set? currParentName (list 'map 'member)) #t)
	      (eq? (isSecondChild node) #t)
	      (eq? (getNodeIsApplication node) #f))
	     #t)
	    (else #f)))))

;; fourth provided type

(define (checkIsSentenceOrWord node)
  (let ((currParent (getParent node))
	(currHaveParent (haveParent node)))
    (let ((currParentName (if (eq? currHaveParent #t)
			      (getNodeName currParent)
			      nil)))
      (cond ((and
	      (eq? (element-of-set? currParentName (list 'first 'butfirst 'sentence 'member?)) #t)
	      (eq? (getNodeIsApplication node) #f))
	     #t)
	    ((and
	      (eq? currParentName 'every)
	      (eq? (isSecondChild node) #t)
	      (eq? (getNodeIsApplication node) #f))
	     #t)
	    (else #f)))))

(define (setTypeBasedOnChecks node)
  (let ((tentativeTypes nil)
	(finalType UNKNOWN_CODE))
    (if (eq? (checkIsProcedure node) #t)
	(set! tentativeTypes (append tentativeTypes (list PROCEDURE_CODE))))
    (if (eq? (checkIsNumber node) #t)
	(set! tentativeTypes (append tentativeTypes (list NUMBER_CODE))))
    (if (eq? (checkIsList node) #t)
	(set! tentativeTypes (append tentativeTypes (list LIST_CODE))))
    (if (eq? (checkIsSentenceOrWord node) #t)
	(set! tentativeTypes (append tentativeTypes (list SENTENCE_OR_WORD_CODE))))
    (if (> (length tentativeTypes) 1)
	(set! finalType CONFLICTING_CODE))
    (if (= (length tentativeTypes) 1)
	(set! finalType (car tentativeTypes)))
    (if (= (length tentativeTypes) 0)
	(set! finalType UNKNOWN_CODE))
    (setNodeTypeCode node finalType)))

; get nodes

(define (convertToNodeHierarchy nestedLists)
  (convertToNodeHierarchyHelper nestedLists nil))

(define (convertToNodeHierarchyHelper nestedLists parentID)
  (if (not (list? nestedLists))
      (let ((name nestedLists))
	(let ((currNode (createNode name #f)))
	  (postProcessNode currNode nil parentID)
	  currNode))
      (let ((currList nestedLists))
	(let ((name (car currList))
	      (remainingElements (cdr currList)))
	  (let ((currNode (createNode name #t)))
	    (let ((childNodes
		   (map (lambda (x)
			  (convertToNodeHierarchyHelper x (getNodeID currNode)))
			remainingElements)))
	      (let ((childNodeIDs (map (lambda (x) (getNodeID x)) childNodes)))
		(postProcessNode currNode childNodeIDs parentID)
		currNode)))))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (makeSet elements)
  (let ((currSet nil))
    (for-each (lambda (x) (set! currSet (adjoin-set x currSet))) elements)
    currSet))

(define (consolidateTypesForGivenNames nodes chosenNames)
  (let ((chosenNameSet (makeSet chosenNames)))
    (for-each (lambda (chosenName)
		(put 'consolidatedTypeCode chosenName UNKNOWN_CODE))
	      chosenNames)
    (consolidateTypesForGivenNamesHelper nodes chosenNameSet)))

; use put/get with keys 'consolidatedTypeCode and name

(define (consolidateTypesForGivenNamesHelper nodes chosenNameSet)
  (for-each (lambda (currNode)
	      (let ((currName (getNodeName currNode))
		    (currTypeCode (getNodeTypeCode currNode)))
		(let ((nextTypeCode currTypeCode))
		  (if (element-of-set? currName chosenNameSet)
		      (let ((prevTypeCode (get 'consolidatedTypeCode currName)))
			(cond ((eq? currTypeCode UNKNOWN_CODE)
			       (set! nextTypeCode prevTypeCode))
			      ((eq? currTypeCode CONFLICTING_CODE)
			       (set! nextTypeCode CONFLICTING_CODE))
			      ((and (not (eq? currTypeCode prevTypeCode))
				    (not (eq? prevTypeCode UNKNOWN_CODE)))
			       (set! nextTypeCode CONFLICTING_CODE)))
			(put 'consolidatedTypeCode currName nextTypeCode))))))
	    nodes))

(define (getConsolidatedTypeCode name)
  (get 'consolidatedTypeCode name))

(define line '(define (foo a b c d e f)
		(f (append (a b) c '(b c)) (+ 5 d) (sentence (first e) f))))

(define rootNode (convertToNodeHierarchy line))

(define flattenedNodes (toFlattenedList rootNode))

; (display flattenedNodes)

(define variableNames (cdr (car (cdr line))))

(for-each (lambda (node) (setTypeBasedOnChecks node)) flattenedNodes)

(consolidateTypesForGivenNames flattenedNodes variableNames)

(define nameTypePairs
  (map (lambda (name) (list name (getTypeStr (getConsolidatedTypeCode name))))
       variableNames))

(display nameTypePairs)

