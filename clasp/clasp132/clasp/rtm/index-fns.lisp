;;; Version 2.1, June 1992.
;;; ***************************************************************************
;;;
;;; Written by: Paul Silvey
;;;             Department of Computer and Information Science
;;;             University of Massachusetts
;;;             Amherst, MA 01003
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;; Copyright (c) 1990 - 1994 University of Massachusetts
;;; Department of Computer and Information Science
;;; Experimental Knowledge Systems Laboratory
;;; Professor Paul Cohen, Director.
;;; All rights reserved.
;;; 
;;; Permission to use, copy, modify and distribute this software and its
;;; documentation is hereby granted without fee for non-commercial uses only 
;;; (not for resale), provided that the above copyright notice of EKSL, this 
;;; paragraph and the one following appear in all copies and in supporting 
;;; documentation.
;;; EKSL makes no representation about the suitability of this software for any
;;; purposes.  It is provided "AS IS", without express or implied warranties
;;; including (but not limited to) all implied warranties of merchantability
;;; and fitness for a particular purpose, and notwithstanding any other
;;; provision contained herein.  In no event shall EKSL be liable for any
;;; special, indirect or consequential damages whatsoever resulting from loss
;;; of use, data or profits, whether in an action of contract, negligence or
;;; other tortuous action, arising out of or in connection with the use or
;;; performance of this software, even if EKSL is
;;; advised of the possiblity of such damages.
;;; 
;;; For more information write to clasp-support@cs.umass.edu
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;; -- * --

#+OLD
(cl:defpackage "RTM"
  (:use "COMMON-LISP"))

(in-package "RTM")

;;; -- * --
;;;
;;; RTM table-attribute indices are threaded 2-3 (balanced) trees.
;;;    Memory space for index nodes is pre-allocated at the time of index creation or expansion.
;;;    Index nodes are dynamically allocated and released, and index grows in size as needed.
;;;

;;; Threaded 2-3 tree table-attribute index node record structure:

(defstruct (index-node
	      (:print-function print-index-node))
  (u nil)     ;; Up
  (l nil)     ;; Left
  (m nil)     ;; Middle
  (r nil)     ;; Right
  (a nil)     ;; First
  (b nil)     ;; Second
  (c nil))    ;; Third   

(defmacro index-node (index node-pos)
  `(aref ,index ,node-pos))

;;;    There are four index node types:
;;;      1. Index header node (only 1, stored in array position 0)
;;;          U: Pointer to first free node
;;;          L: Pointer to minimum attribute leaf node
;;;          M: Pointer to maximum attribute leaf node
;;;          R: Pointer to root node
;;;          A: nil
;;;          B: nil
;;;          C: nil

(defmacro index-first-free-node-pos (index)
  `(index-node-u (index-node ,index 0)))

(defmacro index-min-attr-leaf-pos (index)
  `(index-node-l (index-node ,index 0)))

(defmacro index-max-attr-leaf-pos (index)
  `(index-node-m (index-node ,index 0)))

(defmacro index-root-pos (index)
  `(index-node-r (index-node ,index 0)))

(defmacro index-node-header-p (node)
  `(and (null (index-node-a ,node))
	(null (index-node-b ,node))
	(null (index-node-c ,node))))

;;;       2. Index free node (part of chained set)
;;;          U: Pointer to next free node
;;;          L: nil

(defmacro index-next-free-node-pos (node)
  `(index-node-u ,node))

(defmacro index-node-free-p (node)
  `(and (null (index-node-l ,node))
	(null (index-node-a ,node))
	(null (index-node-b ,node))
	(null (index-node-c ,node))))

;;;       3 & 4: Threaded 2-3 tree nodes
;;;          U: Pointer to parent node (nil for root node)
;;;          A: Pointer to first child or table tuple list
;;;          B: Pointer to second child (nil for leaf nodes)
;;;          C: Pointer to third child

(defmacro index-node-parent-pos (node)
  `(index-node-u ,node))

(defmacro index-node-root-p (node)
  `(null (index-node-parent-pos ,node)))

(defmacro index-node-child1-pos (node)
  `(index-node-a ,node))

(defmacro index-node-child2-pos (node)
  `(index-node-b ,node))

(defmacro index-node-child3-pos (node)
  `(index-node-c ,node))

(defmacro index-node-leaf-p (node)
  `(null (index-node-child2-pos ,node)))

(defmacro index-node-3-children-p (node)
  `(index-node-child3-pos ,node))

(defmacro index-node-number-of-children (node)
  `(if (index-node-child3-pos ,node) 3 2))

;;;       3. Threaded 2-3 tree interior node
;;;          L: Label left
;;;          M: Label middle
;;;          R: Label right

(defmacro index-node-label-left (node)
  `(index-node-l ,node))

(defmacro index-node-label-middle (node)
  `(index-node-m ,node))

(defmacro index-node-label-right (node)
  `(index-node-r ,node))

;;;       4. Threaded 2-3 tree leaf node
;;;          L: Pointer to predecessor leaf node
;;;          M: Pointer to successor leaf node
;;;          R: Attribute
;;;          A: Table tuple list
;;;          B: nil

(defmacro index-node-attr-value (node)
  `(index-node-r ,node))

(defmacro index-node-tuple-list (node)
  `(index-node-a ,node))

(defmacro index-node-pred-leaf-pos (node)
  `(index-node-l ,node))

(defmacro index-node-succ-leaf-pos (node)
  `(index-node-m ,node))

;;; Index node print function (for debugging inspection)

#+comment
(defun print-index-node (node stream level)
  (declare (ignore level))
  (cond ((index-node-free-p node)
	 (format stream "#<Free: free ~d>"
		 (index-next-free-node-pos node)))
	((index-node-header-p node)
	 (format stream "#<Head: free ~d; min ~d; max ~d; root ~d>"
		 (index-next-free-node-pos node)
		 (index-node-label-left node) (index-node-label-middle node)
		 (index-node-label-right node)))
	((index-node-root-p node)
	 (if (index-node-3-children-p node)
	     (format stream "#<Root: left ~s @ ~d; middle ~s @ ~d; right ~s @ ~d>"
		     (index-node-label-left node) (index-node-child1-pos node)
		     (index-node-label-middle node) (index-node-child2-pos node)
		     (index-node-label-right node) (index-node-child3-pos node))
	     (format stream "#<Root: left ~s @ ~d; middle ~s @ ~d>"
		     (index-node-label-left node) (index-node-child1-pos node)
		     (index-node-label-middle node) (index-node-child2-pos node))))
	((index-node-leaf-p node)
	 (format stream "#<Leaf: parent ~d; attr ~s; tuples ~d; pred ~d; succ ~d>"
		     (index-node-parent-pos node)
		     (index-node-attr-value node) (index-node-tuple-list node)
		     (index-node-pred-leaf-pos node) (index-node-succ-leaf-pos node)))
	(t
	 (if (index-node-3-children-p node)
	     (format stream "#<Node: parent ~d; left ~s @ ~d; middle ~s @ ~d; right ~s @ ~d>"
		     (index-node-parent-pos node)
		     (index-node-label-left node) (index-node-child1-pos node)
		     (index-node-label-middle node) (index-node-child2-pos node)
		     (index-node-label-right node) (index-node-child3-pos node))
	     (format stream "#<Node: parent ~d; left ~s @ ~d; middle ~s @ ~d>"
		     (index-node-parent-pos node)
		     (index-node-label-left node) (index-node-child1-pos node)
		     (index-node-label-middle node) (index-node-child2-pos node))))))

;;; Index creation function:

(defun index-init (initial-size)
  ;; Indices are adjustable arrays that grow in size as necessary during dynamic node allocation.
  (let ((index (make-array initial-size :adjustable t)))
    ;; Initialize index nodes and free node list.
    (do ((node-pos 0 (1+ node-pos))
	 (node (make-index-node) (make-index-node)))
	((>= node-pos (1- initial-size))
	 (setf (index-node index node-pos) node
	       (index-next-free-node-pos node) 0))
      (setf (index-node index node-pos) node 
	    (index-next-free-node-pos node) (1+ node-pos)))
    (setf (index-first-free-node-pos index) 1)
    index))

;;; Miscellaneous index macros and functions:

(defmacro index-node-label-down (index node-pos)
  ;; Node's label is in label-right (same as attr-value slot for leaf nodes).
  `(index-node-label-right (index-node ,index ,node-pos)))

(defmacro index-root-init (index root-pos)
  ;; Initialize a new root node.
  (let ((root (gensym "ROOT")))
    `(let ((,root (index-node ,index ,root-pos)))
       (setf (index-node-parent-pos ,root) nil
	     (index-root-pos ,index) ,root-pos))))

(defmacro index-leaf-init (index leaf-pos attr-value tuple-number)
  ;; Initialize a newly allocated leaf node.
  (let ((leaf (gensym "LEAF")))
    `(let ((,leaf (index-node ,index ,leaf-pos)))
       (setf (index-node-attr-value ,leaf) ,attr-value
	     (index-node-tuple-list ,leaf) (list ,tuple-number)
	     (index-node-child2-pos ,leaf) nil
	     (index-node-child3-pos ,leaf) nil))))

(defmacro index-leaf-thread-between (index new-leaf-pos pred-leaf-pos succ-leaf-pos)
  ;; Thread in new-leaf node between pred-leaf and succ-leaf nodes.
  (let ((new-leaf (gensym "NEW-LEAF")))
    `(let ((,new-leaf (index-node ,index ,new-leaf-pos)))
       ,(if pred-leaf-pos
	    `(if ,pred-leaf-pos 
		 ;; If there is a predecessor, make new-leaf its successor.
		 (setf (index-node-succ-leaf-pos (index-node ,index ,pred-leaf-pos))
		       ,new-leaf-pos)
		 ;; Otherwise, new-leaf node is new index minimum attr-value.
		 (setf (index-min-attr-leaf-pos ,index) ,new-leaf-pos))
	    `(setf (index-min-attr-leaf-pos ,index) ,new-leaf-pos))
       ;; Update new-leaf node threads.
       (setf (index-node-pred-leaf-pos ,new-leaf) ,pred-leaf-pos
	     (index-node-succ-leaf-pos ,new-leaf) ,succ-leaf-pos)
       ,(if succ-leaf-pos
	    `(if ,succ-leaf-pos 
		 ;; If there is a successor, make new-leaf its predecessor.
		 (setf (index-node-pred-leaf-pos (index-node ,index ,succ-leaf-pos))
		       ,new-leaf-pos)
		 ;; Otherwise, new-leaf node is new index maximum attr-value.
		 (setf (index-max-attr-leaf-pos ,index) ,new-leaf-pos))
	    `(setf (index-max-attr-leaf-pos ,index) ,new-leaf-pos)))))

(defmacro index-leaf-unthread (index leaf-pos)
  ;; Unthread leaf node from its predecessor leaf and successor leaf nodes.
  (let ((pred-leaf-pos (gensym "PRED-POS"))
	(succ-leaf-pos (gensym "SUCC-POS")))
    `(let ((,pred-leaf-pos (index-node-pred-leaf-pos (index-node ,index ,leaf-pos)))
	   (,succ-leaf-pos (index-node-succ-leaf-pos (index-node ,index ,leaf-pos))))
       (if ,pred-leaf-pos 
	   ;; If there is a predecessor, make succ-leaf its successor.
	   (setf (index-node-succ-leaf-pos (index-node ,index ,pred-leaf-pos))
		 ,succ-leaf-pos)
	   ;; Otherwise, succ-leaf node is new index minimum attr-value.
	   (setf (index-min-attr-leaf-pos ,index) ,succ-leaf-pos))
       (if ,succ-leaf-pos 
	   ;; If there is a successor, make pred-leaf its predecessor.
	   (setf (index-node-pred-leaf-pos (index-node ,index ,succ-leaf-pos))
		 ,pred-leaf-pos)
	   ;; Otherwise, pred-leaf node is new index maximum attr-value.
	   (setf (index-max-attr-leaf-pos ,index) ,pred-leaf-pos)))))

(defmacro index-link-children (index parent-pos child1-pos child2-pos
				  &optional (child3-pos nil))
  ;; Connect children to parent, parent to children, and update parent labels.
  (let ((parent (gensym "PARENT"))
	(label-middle (gensym "LABEL-MIDDLE"))
	(label-right (gensym "LABEL-RIGHT")))
    `(let* ((,parent (index-node ,index ,parent-pos))
	    (,label-middle (index-node-label-down ,index ,child2-pos))
	    (,label-right ,(if child3-pos
			      `(index-node-label-down ,index ,child3-pos)
			      label-middle)))
       (setf (index-node-parent-pos (index-node ,index ,child1-pos)) ,parent-pos
	     (index-node-parent-pos (index-node ,index ,child2-pos)) ,parent-pos
	     ,@(when child3-pos
		 `((index-node-parent-pos (index-node ,index ,child3-pos)) ,parent-pos))
	     (index-node-child1-pos ,parent) ,child1-pos
	     (index-node-child2-pos ,parent) ,child2-pos
	     (index-node-child3-pos ,parent) ,child3-pos
	     (index-node-label-left ,parent) (index-node-label-down ,index ,child1-pos)
	     (index-node-label-middle ,parent) ,label-middle
	     (index-node-label-right ,parent) ,label-right))))

(defmacro index-node-insert-tuple (leaf tuple-number)
  ;; Add tuple number to list of leaf node.
  `(push ,tuple-number (index-node-tuple-list ,leaf)))

(defmacro index-node-delete-tuple (leaf tuple-number)
  ;; Remove tuple number from list of leaf node.
  (let ((tuple-list (gensym "TUPLE-LIST"))
	(old-tuple-number (gensym "OLD-TUPLE-NUMBER")))
    `(let ((,tuple-list nil))
       (dolist (,old-tuple-number (index-node-tuple-list ,leaf))
	 (unless (= ,old-tuple-number ,tuple-number)
	   (push ,old-tuple-number ,tuple-list)))
       (setf (index-node-tuple-list ,leaf) ,tuple-list))))

(defun index-node-child-pos (node child-num)
  ;; Return the index position of the child specified by child-num.
  (case child-num
    (1 (index-node-child1-pos node))
    (2 (index-node-child2-pos node))
    (3 (index-node-child3-pos node)) 
    (t (rtm-error "Invalid child number requested in call to index-node-child-pos."))))

(defun index-node-child-num (node child-pos)
  ;; Return the number of the child among node's children that has index position child-pos.
  (cond ((= child-pos (index-node-child1-pos node)) 1)
	((= child-pos (index-node-child2-pos node)) 2)
	((= child-pos (index-node-child3-pos node)) 3)
	(t (rtm-error "Invalid child position requested in call to index-node-child-num."))))

(defun index-node-descendant-pos (node attr-value equal-p-fn less-p-fn)
  ;; Return the index position of the child of node where attr-value belongs.
  (cond ((attr-le-p attr-value
		    (index-node-label-left node) equal-p-fn less-p-fn)
	 (index-node-child1-pos node))
	((attr-le-p attr-value
		    (index-node-label-middle node) equal-p-fn less-p-fn)
	 (index-node-child2-pos node))
	((attr-le-p attr-value
		    (index-node-label-right node) equal-p-fn less-p-fn)
	 (index-node-child3-pos node))
	(t nil)))

(defun index-node-descendant-num (node attr-value equal-p-fn less-p-fn)
  ;; Return the rank ordering position of the child of node where attr-value belongs.
  (cond ((attr-le-p attr-value
		    (index-node-label-left node) equal-p-fn less-p-fn)
	 1)
	((attr-le-p attr-value
		    (index-node-label-middle node) equal-p-fn less-p-fn)
	 2)
	((or (not (index-node-3-children-p node))
	     (attr-le-p attr-value
			(index-node-label-right node) equal-p-fn less-p-fn))
	 3)
	(t
	 4)))

(defun index-node-pred-sib-pos (index node-pos)
  ;; Return the index position of the predecessor sibling node (if any).
  (let* ((parent-pos (index-node-parent-pos (index-node index node-pos)))
	 (parent (index-node index parent-pos))
	 (pred-num (1- (index-node-child-num parent node-pos))))
    (when (> pred-num 0)
      (index-node-child-pos parent pred-num)))) 

(defun index-node-succ-sib-pos (index node-pos)
  ;; Return the index position of the successor sibling node (if any).  
  (let* ((parent-pos (index-node-parent-pos (index-node index node-pos)))
	 (parent (index-node index parent-pos))
	 (succ-num (1+ (index-node-child-num parent node-pos))))
    (when (< succ-num 4)
      (index-node-child-pos parent succ-num))))

;;;
;;;  RTM index node maintenance functions.
;;;

(defun index-label-update (index parent-pos child-pos label)
  ;; Update label of parent, and propagate upward as needed.
  ;; Iterative algorithm is used for efficiency.
  (let (parent child-num)
    (do* ((c-pos child-pos p-pos)
	  (p-pos parent-pos (index-node-parent-pos parent)))
	 ((null p-pos))
      (setf parent (index-node index p-pos)
	    child-num (index-node-child-num parent c-pos))
      (case child-num 
	(1 (setf (index-node-label-left parent) label)
	   (return))
	(2 (setf (index-node-label-middle parent) label)
	   (if (index-node-3-children-p parent)
	       (return)
	       (setf (index-node-label-right parent) label)))
	(3 (setf (index-node-label-right parent) label))))))
  
(defun index-node-allocate (attribute)
  ;; Allocate first available node and return its position if successful or nil if error.
  (let* ((index (attribute-index attribute))
	 (node-pos (index-first-free-node-pos index)))
    ;; If no index record positions are available, expand the index array by 50%.
    (when (zerop node-pos)
      (let* ((old-size (array-total-size index))
	     (new-size (round (* old-size *rtm-table-expansion-factor*))))
	(when (> new-size array-total-size-limit)
	  (rtm-error "Table-attribute index array too large for expansion.")
	  (return-from index-node-allocate nil))
	;; If not too big already, expand index, initializing new index nodes and free node list,
	;; and update attribute record.
	(setf index (adjust-array index (list new-size)))
	(do ((node-pos old-size (1+ node-pos))
	     (node (make-index-node) (make-index-node)))
	    ((>= node-pos (1- new-size))
	     (setf (index-node index node-pos) node
		   (index-next-free-node-pos node) 0))
	  (setf (index-node index node-pos) node 
		(index-next-free-node-pos node) (1+ node-pos)))
	(setf (attribute-index attribute) index)
	(setf node-pos old-size)))
    ;; Update free node list, and return node position.
    (setf (index-first-free-node-pos index)
	  (index-next-free-node-pos (index-node index node-pos)))
    node-pos))

(defun index-node-release (attribute node-pos)
  ;; Release specified node and update free node list.
  (let* ((index (attribute-index attribute))
	 (node (index-node index node-pos)))
    ;; Update free node list, by linking node in at head of chain.
    (setf (index-node-label-left node) nil
	  (index-node-child1-pos node) nil
	  (index-node-child2-pos node) nil
	  (index-node-child3-pos node) nil)
    (setf (index-next-free-node-pos node) (index-first-free-node-pos index))
    (setf (index-first-free-node-pos index) node-pos)))

;;;
;;; RTM index insertion and deletion functions.
;;;

(defun index-insert (attr-name attr-value tuple-number)
  (let* ((attribute (current-attribute attr-name))
         (index (attribute-index attribute))
         (root-pos (when index (index-root-pos index))))
    (if root-pos
      ;; If there is a root node, see if it is an interior or a leaf node.
      (let* ((root (index-node index root-pos))
             (root-attr-value (index-node-attr-value root))
             (domain (current-domain attr-name))
             (equal-p-fn (domain-equal-p domain))
             (less-p-fn (domain-less-p domain)))
        (if (index-node-leaf-p root)
          ;; If the root node is a leaf node, then the 2-3 tree consists of a single vertex.
          (if (funcall equal-p-fn attr-value root-attr-value)
            ;; If the new attr-value is the same as this node's value, 
            ;;    simply update node's tuple list.
            (index-node-insert-tuple root tuple-number)
            ;; If the new attr-value is different from this node's value, create a sibling node "leaf" and
            ;; a parent node "vert" for both that then becomes the new root node.
            (let ((leaf-pos (index-node-allocate attribute))
                  (vert-pos (index-node-allocate attribute))
                  (index (attribute-index attribute)))
              (index-leaf-init index leaf-pos attr-value tuple-number)
              ;; Determine correct order of nodes and initialize them.
              (if (funcall less-p-fn attr-value root-attr-value)
                (progn
                  ;; Order is leaf, root. Update leaf node threads.
                  (index-link-children index vert-pos leaf-pos root-pos)
                  (index-leaf-thread-between index leaf-pos nil root-pos))
                (progn
                  ;; Order is root, leaf. Update leaf node threads.
                  (index-link-children index vert-pos root-pos leaf-pos)
                  (index-leaf-thread-between index leaf-pos root-pos nil)))
              ;; Make vert become the new root.
              (index-root-init index vert-pos)))
          ;; If the root node is an interior node, first traverse the tree using index-find.
          (let* ((node-pos (index-find index attr-value equal-p-fn less-p-fn))
                 (node (index-node index node-pos)))
            (if (index-node-leaf-p node)
              ;; If the node found is a leaf, then simply update its tuple-list.
              (index-node-insert-tuple node tuple-number)
              ;; Otherwise, the node found is the parent node where attr-value belongs as a leaf.
              ;;    Determine its correctly ordered child position, then allocate and initialize 
              ;;    a new leaf node. Use index-addchild to insert new leaf and rebalance the 
              ;;    2-3 tree if necessary.
              (let ((leaf-num (index-node-descendant-num node attr-value
                                                         equal-p-fn less-p-fn))
                    (leaf-pos (index-node-allocate attribute))
                    (index (attribute-index attribute)))
                ;; Initialize leaf node and insert it in the 2-3 tree.
                (index-leaf-init index leaf-pos attr-value tuple-number)
                (index-addchild attribute node-pos leaf-pos leaf-num))))))
      ;; If there is no root node in index, 2-3 tree is empty.
      ;; Create and initialize a single leaf node and assign it to be root.
      (let ((leaf-pos (index-node-allocate attribute))
            (index (attribute-index attribute)))
        (index-leaf-init index leaf-pos attr-value tuple-number)
        (index-leaf-thread-between index leaf-pos nil nil)
        (index-root-init index leaf-pos)))))

(defun index-addchild (attribute parent-pos child-pos child-num)
  ;; Add a 2-3 tree child node to parent node at the specified child number (rank).
  ;; If parent already has 3 children, recursively call index-addchild to rebalance the 2-3 tree.
  (let* ((index (attribute-index attribute))
	 (parent (index-node index parent-pos))
	 (sib1-pos (index-node-child1-pos parent))
	 (sib2-pos (index-node-child2-pos parent))
	 (sib3-pos (index-node-child3-pos parent))
	 (child-leaf-p (index-node-leaf-p (index-node index child-pos)))
	 pred-pos succ-pos)
    (if (null sib3-pos)
	;; Parent currently has only 2 children, so there's room to add child without rebalancing.
	;; Place child in its correct position with respect to its siblings.
	(case child-num
	  (1 ;; Order is child, sib1, sib2.  Update leaf node threads if appropriate.
	   (index-link-children index parent-pos child-pos sib1-pos sib2-pos)
	   (when child-leaf-p 
	     (setf pred-pos (index-node-pred-leaf-pos (index-node index sib1-pos)))
	     (index-leaf-thread-between index child-pos pred-pos sib1-pos)))
	  (2 ;; Order is sib1, child, sib2.  Update leaf node threads if appropriate.
	   (index-link-children index parent-pos sib1-pos child-pos sib2-pos)
	   (when child-leaf-p
	     (index-leaf-thread-between index child-pos sib1-pos sib2-pos)))
	  (3 ;; Order is sib1, sib2, child.  Update leaf node threads if appropriate.
	   (index-link-children index parent-pos sib1-pos sib2-pos child-pos)
	   (when child-leaf-p
	     (setf succ-pos (index-node-succ-leaf-pos (index-node index sib2-pos)))
	     (index-leaf-thread-between index child-pos sib2-pos succ-pos))
	   ;; Propagate new right label.
	   (index-label-update index (index-node-parent-pos parent)
			      parent-pos (index-node-label-down index child-pos)))
	  (otherwise (rtm-error "Table-attribute index has invalid form - child number "
				 child-num " reported.")))
	;; Parent already has 3 children, so new child must be added by creating a new parent, "node".
	(let ((node-pos (index-node-allocate attribute))
	      grandparent-pos)
	  ;; Reaccess index, in case it has changed (possible during expansion in node allocation).
	  (setf index (attribute-index attribute))
	  ;; Place child in its correct position with respect to its siblings.
	  (case child-num
	    (1 ;; Order is child, sib1, sib2, sib3.  Update leaf node threads if appropriate.
	     (index-link-children index parent-pos child-pos sib1-pos)
	     (index-link-children index node-pos sib2-pos sib3-pos)
	     (when child-leaf-p
	       (setf pred-pos (index-node-pred-leaf-pos (index-node index sib1-pos)))
	       (index-leaf-thread-between index child-pos pred-pos sib1-pos)))
	    (2 ;; Order is sib1, child, sib2, sib3.  Update leaf node threads if appropriate.
	     (index-link-children index parent-pos sib1-pos child-pos)
	     (index-link-children index node-pos sib2-pos sib3-pos)
	     (when child-leaf-p
	       (index-leaf-thread-between index child-pos sib1-pos sib2-pos)))
	    (3 ;; Order is sib1, sib2, child, sib3.  Update leaf node threads if appropriate.
	     (index-link-children index parent-pos sib1-pos sib2-pos)
	     (index-link-children index node-pos child-pos sib3-pos)
	     (when child-leaf-p
	       (index-leaf-thread-between index child-pos sib2-pos sib3-pos)))
	    (4 ;; Order as sib1, sib2, sib3, child.  Update leaf node threads if appropriate.
	     (index-link-children index parent-pos sib1-pos sib2-pos)
	     (index-link-children index node-pos sib3-pos child-pos)
	     (when child-leaf-p
	       (setf succ-pos (index-node-succ-leaf-pos (index-node index sib3-pos)))
	       (index-leaf-thread-between index child-pos sib3-pos succ-pos)))
	    (otherwise (rtm-error "Table-attribute index has invalid form - child number "
				   child-num " reported.")))
	  ;; Insert node in the 2-3 tree, creating a new root if necessary.
	  (setf grandparent-pos (index-node-parent-pos (index-node index parent-pos)))
	  (if grandparent-pos
	      ;; Recursively add child one level up the tree.
	      (let ((node-num (1+ (index-node-child-num
				    (index-node index grandparent-pos) parent-pos))))
		(index-addchild attribute grandparent-pos node-pos node-num)
		;; Reaccess index, in case it has changed 
                ;;    (possible during expansion in adding child).
		(setf index (attribute-index attribute))
		;; Propagate new labels up.
		(index-label-update
		  index (index-node-parent-pos (index-node index parent-pos))
		  parent-pos (index-node-label-down index parent-pos))
		(index-label-update
		  index (index-node-parent-pos (index-node index node-pos))
		  node-pos (index-node-label-down index node-pos)))
	      ;; At top, create a new root with "parent" and "node" as children.
	      (let ((root-pos (index-node-allocate attribute)))
		;; Reaccess index, in case it has changed 
                ;;    (possible during expansion in allocating new root).
		(setf index (attribute-index attribute))
		(index-link-children index root-pos parent-pos node-pos)
		(index-root-init index root-pos)))))))

(defun index-delete (attr-name attr-value tuple-number)
  ;; Find leaf node with attr-value so tuple-number can be removed from its tuple-list.
  (let* ((attribute (current-attribute attr-name))
	 (index (attribute-index attribute))
	 (domain (current-domain attr-name))
	 (equal-p-fn (domain-equal-p domain))
	 (less-p-fn (domain-less-p domain))
	 (node-pos (index-find index attr-value equal-p-fn less-p-fn))
	 (node (index-node index node-pos))
	 (parent-pos (index-node-parent-pos node)))
    ;; Delete tuple-number from tuple-list.
    (index-node-delete-tuple node tuple-number)
    (unless (index-node-tuple-list node)
      ;; If tuple-list is now empty, delete whole index node.
      (if parent-pos
	  ;; If node is not root, delete node using index-delchild to rebalance the 2-3 tree.
	  (index-delchild attribute parent-pos node-pos)
	  ;; Otherwise, node is root, so release and update index header.
	  (progn
	    (index-node-release attribute node-pos)
	    (setf (index-min-attr-leaf-pos index) nil
		  (index-max-attr-leaf-pos index) nil
		  (index-root-pos index) nil))))))

(defun index-delchild (attribute parent-pos child-pos)
  ;; Delete a 2-3 tree child node from parent node.
  ;; If parent only has 2 children, recursively call index-delchild to rebalance the 2-3 tree.
  (let* ((index (attribute-index attribute))
	 (parent (index-node index parent-pos))
	 (child-num (index-node-child-num parent child-pos))
	 (sib1-pos (index-node-child1-pos parent))
	 (sib2-pos (index-node-child2-pos parent))
	 (sib3-pos (index-node-child3-pos parent))
	 (child-leaf-p (index-node-leaf-p (index-node index child-pos)))
	 pred-pos succ-pos)
    (if sib3-pos
	;; Parent has 3 children, so child can be deleted without rebalancing.
	(case child-num
	  (1 ;; Order is child, sib2, sib3.  Update leaf node threads if appropriate.
	   (index-link-children index parent-pos sib2-pos sib3-pos)
	   (when child-leaf-p 
	     (setf pred-pos (index-node-pred-leaf-pos (index-node index child-pos)))
	     (index-leaf-thread-between index sib2-pos pred-pos sib3-pos)))
	  (2 ;; Order is sib1, child, sib3.  Update leaf node threads if appropriate.
	   (index-link-children index parent-pos sib1-pos sib3-pos)
	   (when child-leaf-p
	     (setf pred-pos (index-node-pred-leaf-pos (index-node index sib1-pos)))
	     (index-leaf-thread-between index sib1-pos pred-pos sib3-pos)
	     (setf succ-pos (index-node-succ-leaf-pos (index-node index sib3-pos)))
	     (index-leaf-thread-between index sib3-pos sib1-pos succ-pos)))
	  (3 ;; Order is sib1, sib2, child.  Update leaf node threads if appropriate.
	   (index-link-children index parent-pos sib1-pos sib2-pos)
	   (when child-leaf-p
	     (setf succ-pos (index-node-succ-leaf-pos (index-node index child-pos)))
	     (index-leaf-thread-between index sib2-pos sib1-pos succ-pos))
	   ;; Propagate new right label.
	   (index-label-update index (index-node-parent-pos parent)
			      parent-pos (index-node-label-down index sib2-pos)))
	  (otherwise (rtm-error "Table-attribute index has invalid form - child number "
				 child-num " reported.")))
	;; Parent only has 2 children, so 2-3 tree will need rebalancing after deleting child
	;; since all nodes need at least 2 children.  Parent must be also be deleted and the
	;; orphaned sibling of child must be adopted by a sibling of parent.
	(let ((grandparent-pos (index-node-parent-pos parent))
	      (orphan-pos (case child-num (1 sib2-pos) (2 sib1-pos))))
	  (if grandparent-pos
	      ;; Use grandparent node to find adoptive parent for orphan node.
	      (let ((parent-sib-pos (index-node-succ-sib-pos index parent-pos))
		    (orphan-num 1))
		(unless parent-sib-pos
		  (setf parent-sib-pos (index-node-pred-sib-pos index parent-pos))
		  (incf orphan-num (index-node-number-of-children
				     (index-node index parent-sib-pos))))
		(when child-leaf-p
		  (index-leaf-unthread index child-pos)
		  (index-leaf-unthread index orphan-pos))
		(index-delchild attribute grandparent-pos parent-pos)
		(index-addchild attribute parent-sib-pos orphan-pos orphan-num))
	      ;; No grandparent, so orphan becomes the new root node.
	      (progn
		(when (index-node-leaf-p (index-node index orphan-pos))
		  (index-leaf-thread-between index orphan-pos nil nil))
		(index-root-init index orphan-pos)
		(index-node-release attribute parent-pos)))))
    (index-node-release attribute child-pos)))

;;;
;;; RTM index query functions.
;;;

(defun index-find (index attr-value equal-p-fn less-p-fn)
  ;; Find leaf index node whose leaf-attr-value equals attr-value, or the interior node that should
  ;; have attr-value as a child.  Return index node position.  If index has no root node, return nil.
  (let ((node-pos (index-root-pos index)) node)
    (when node-pos
      (let ((pred-attr-value (index-node-label-right (index-node index node-pos))))
	(if (attr-gt-p attr-value pred-attr-value equal-p-fn less-p-fn)
	    ;; If attr-value is greater than any in index, return parent-pos of max-attr-leaf node.
	    (setf node-pos (index-node-parent-pos
			     (index-node index (index-max-attr-leaf-pos index))))
	    ;; Otherwise, find using iterative traversal for efficiency. 
	    (loop
	      (setf node (index-node index node-pos))
	      (when (index-node-leaf-p node)
		;; If node is a leaf node, see if attr-values match and return either node-pos or its parent's pos.
		(unless (funcall equal-p-fn attr-value (index-node-attr-value node))
		  (setf node-pos (index-node-parent-pos node)))
		(return))
	      ;; If node is an interior node, follow the correct child path.
	      (setf node-pos (index-node-descendant-pos node attr-value
							equal-p-fn less-p-fn))))))
    node-pos))

(defun index-union-interval-thread (tuple-set index start-pos end-pos)
  ;; Union in all tuple numbers in closed interval along thread from start node to end node.
  ;; If either start-pos or end-pos is nil, use min-attr-leaf-pos or max-attr-leaf-pos respectively.
  (unless start-pos (setf start-pos (index-min-attr-leaf-pos index)))
  (unless end-pos (setf end-pos (index-max-attr-leaf-pos index)))
  (do* ((node-pos start-pos (index-node-succ-leaf-pos node))
	(node (index-node index node-pos) (index-node index node-pos)))
       ((eql node-pos end-pos)
	(tuple-set-union-list tuple-set (index-node-tuple-list node)))
    (tuple-set-union-list tuple-set (index-node-tuple-list node))))
	
(defun index-query-eq (tuple-set index attr-value equal-p-fn less-p-fn)
  ;; Union appropriate tuple numbers (if any) into tuple-set.
  (let* ((root-pos (index-root-pos index))
	 (root (when root-pos (index-node index root-pos))))
    (when root
      ;; Index has at least one node.
      (if (index-node-leaf-p root)
	  ;; If Index has only one leaf node, compare attr-values.
	  (when (funcall equal-p-fn (index-node-attr-value root) attr-value)
	    (tuple-set-union-list tuple-set (index-node-tuple-list root)))
	  ;; Otherwise, find matching leaf node or appropriate parent.
	  (let* ((node-pos (index-find index attr-value equal-p-fn less-p-fn))
		 (node (index-node index node-pos)))
	    ;; If matching attribute-value leaf node found, union in its tuple-list.
	    (when (index-node-leaf-p node)
	      (tuple-set-union-list tuple-set (index-node-tuple-list node))))))
    tuple-set))

(defun index-query-ne (tuple-set index attr-value equal-p-fn less-p-fn)
   ;; Union appropriate tuple numbers into tuple-set.
  (let* ((root-pos (index-root-pos index))
	 (root (when root-pos (index-node index root-pos))))
    (when root
      ;; Index has at least one node.
      (if (index-node-leaf-p root)
	  ;; If Index has only one leaf node, compare attr-values.
	  (unless (funcall equal-p-fn (index-node-attr-value root) attr-value)
	    (tuple-set-union-list tuple-set (index-node-tuple-list root)))
	  ;; Otherwise, find matching leaf node or appropriate parent.
	  (let* ((node-pos (index-find index attr-value equal-p-fn less-p-fn))
		 (node (index-node index node-pos)))
	    (if (index-node-leaf-p node)
		;; If matching attribute-value leaf node found, use its predecessor
		;; and successor leaf nodes to delineate excluding intervals.
		(let ((end-pos (index-node-pred-leaf-pos node))
		      (start-pos (index-node-succ-leaf-pos node)))
		  ;; If end-pos in index, union in tuple-lists along thread from start of index to end-pos.
		  (when end-pos
		    (index-union-interval-thread tuple-set index nil end-pos))
		  ;; If start-pos in index, union in tuple-lists along thread from start-pos to end of index.
		  (when start-pos
		    (index-union-interval-thread tuple-set index start-pos nil)))
		;; Otherwise, not found, so union in tuple-lists along thread through whole index.
		(index-union-interval-thread tuple-set index nil nil)))))
    tuple-set))

(defun index-query-lt (tuple-set index attr-value equal-p-fn less-p-fn)
  ;; Union appropriate tuple numbers into tuple-set.
  (let* ((root-pos (index-root-pos index))
         (root (when root-pos (index-node index root-pos))))
    (when root
      ;; Index has at least one node.
      (if (index-node-leaf-p root)
        ;; If Index has only one leaf node, compare attr-values.
        (when (funcall less-p-fn (index-node-attr-value root) attr-value)
          (tuple-set-union-list tuple-set (index-node-tuple-list root)))
        ;; Otherwise, find matching leaf node or appropriate parent.
        (let* ((node-pos (index-find index attr-value equal-p-fn less-p-fn))
               (node (index-node index node-pos))
               end-pos)
          (if (index-node-leaf-p node)
            ;; If matching attribute-value leaf node found, 
            ;;    use its predecessor as end node.
            (setf end-pos (index-node-pred-leaf-pos node))
            ;; Otherwise, get leaf that is next greatest (if any) 
            ;;    and use its predecessor as end node.
            (let ((leaf-pos (index-node-descendant-pos node attr-value
                                                       equal-p-fn less-p-fn)))
              (if leaf-pos
                ;; If next greatest leaf node exists, use its predecessor as end node.
                (setf end-pos (index-node-pred-leaf-pos (index-node index leaf-pos)))
                ;; Otherwise, use max-attr-leaf as end node.
                (setf end-pos (index-max-attr-leaf-pos index)))))
          ;; If end-pos in index, union in tuple-lists along thread 
          ;;    from start of index to end-pos.
          (when end-pos
            (index-union-interval-thread tuple-set index nil end-pos)))))
    tuple-set))

(defun index-query-ge (tuple-set index attr-value equal-p-fn less-p-fn)
  ;; Union appropriate tuple numbers into tuple-set.
  (let* ((root-pos (index-root-pos index))
	 (root (when root-pos (index-node index root-pos))))
    (when root
      ;; Index has at least one node.
      (if (index-node-leaf-p root)
	  ;; If Index has only one leaf node, compare attr-values.
	  (unless (funcall less-p-fn (index-node-attr-value root)  attr-value)
	    (tuple-set-union-list tuple-set (index-node-tuple-list root)))
	  ;; Otherwise, find matching leaf node or appropriate parent.
	  (let* ((node-pos (index-find index attr-value equal-p-fn less-p-fn))
		 (node (index-node index node-pos))
		 start-pos)
	    (if (index-node-leaf-p node)
		;; If matching attribute-value leaf node found, use it as start node.
		(setf start-pos node-pos)
		;; Otherwise, get leaf that is next greatest (if any) 
                ;;    and use it as start node.
		(setf start-pos (index-node-descendant-pos node attr-value
							   equal-p-fn less-p-fn)))
	    (when start-pos
	      ;; If start-pos in index, union in tuple-lists along thread 
              ;;    from start-pos to end of index.
	      (index-union-interval-thread tuple-set index start-pos nil)))))
    tuple-set))

(defun index-query-gt (tuple-set index attr-value equal-p-fn less-p-fn)
  ;; Union appropriate tuple numbers into tuple-set.
  (let* ((root-pos (index-root-pos index))
	 (root (when root-pos (index-node index root-pos))))
    (when root
      ;; Index has at least one node.
      (if (index-node-leaf-p root)
	  ;; If Index has only one leaf node, compare attr-values.
	  (let ((root-value (index-node-attr-value root)))
	    (when (attr-gt-p root-value attr-value equal-p-fn less-p-fn)
	      (tuple-set-union-list tuple-set (index-node-tuple-list root))))
	  ;; Otherwise, find matching leaf node or appropriate parent.
	  (let* ((node-pos (index-find index attr-value equal-p-fn less-p-fn))
		 (node (index-node index node-pos))
		 start-pos)
	    (if (index-node-leaf-p node)
		;; If matching attribute-value leaf node found, use its successor as start node.
		(setf start-pos (index-node-succ-leaf-pos node))
		;; Otherwise, get leaf that is next greatest (if any) and use it as start node.
		(setf start-pos (index-node-descendant-pos node attr-value
							   equal-p-fn less-p-fn)))
	    (when start-pos
	      ;; If start-pos in index, union in tuple-lists along thread 
              ;;    from start-pos to end of index.
	      (index-union-interval-thread tuple-set index start-pos nil)))))
    tuple-set))

(defun index-query-le (tuple-set index attr-value equal-p-fn less-p-fn)
  ;; Union appropriate tuple numbers into tuple-set.
  (let* ((root-pos (index-root-pos index))
	 (root (when root-pos (index-node index root-pos))))
    (when root
      ;; Index has at least one node.
      (if (index-node-leaf-p root)
	  ;; If Index has only one leaf node, compare attr-values.
	  (let ((root-value (index-node-attr-value root)))
	    (when (attr-le-p root-value attr-value equal-p-fn less-p-fn)
	      (tuple-set-union-list tuple-set (index-node-tuple-list root))))
	  ;; Otherwise, find matching leaf node or appropriate parent.
	  (let* ((node-pos (index-find index attr-value equal-p-fn less-p-fn))
		 (node (index-node index node-pos))
		 end-pos)
	    (if (index-node-leaf-p node)
		;; If matching attribute-value leaf node found, use it as end node.
		(setf end-pos node-pos)
		;; Otherwise, get leaf that is next greatest (if any) 
                ;;    and use its predecessor as end node.
		(let ((leaf-pos (index-node-descendant-pos node attr-value
							   equal-p-fn less-p-fn)))
		  (if leaf-pos
		      ;; If next greatest leaf node exists, use its predecessor as end node.
		      (setf end-pos (index-node-pred-leaf-pos (index-node index leaf-pos)))
		      ;; Otherwise, use max-attr-leaf as end node.
		      (setf end-pos (index-max-attr-leaf-pos index)))))
	    ;; If end-pos in index, union in tuple-lists along thread 
            ;;    from start of index to end-pos.	    
	    (when end-pos
	      (index-union-interval-thread tuple-set index nil end-pos)))))
    tuple-set))

(defun index-query-all (tuple-set index)
  ;; Union appropriate tuple numbers into tuple-set.
  (let* ((root-pos (index-root-pos index))
	 (root (when root-pos (index-node index root-pos))))
    (when root
      ;; Index has at least one node, so thread through whole index.
      (index-union-interval-thread tuple-set index nil nil))
    tuple-set))

(defun index-query-max (tuple-set index)
  ;; Union appropriate tuple numbers into tuple-set.
  (let* ((root-pos (index-root-pos index))
	 (root (when root-pos (index-node index root-pos))))
    (when root
      ;; Index has at least one node, so get tuple-numbers of max attr.
      (tuple-set-union-list
	tuple-set (index-node-tuple-list
		    (index-node index (index-max-attr-leaf-pos index)))))
    tuple-set))

(defun index-query-min (tuple-set index)
  ;; Union appropriate tuple numbers into tuple-set.
  (let* ((root-pos (index-root-pos index))
	 (root (when root-pos (index-node index root-pos))))
    (when root
      ;; Index has at least one node, so get tuple-numbers of min attr.
      (tuple-set-union-list
	tuple-set (index-node-tuple-list
		    (index-node index (index-min-attr-leaf-pos index)))))
    tuple-set))

(defun index-query-pred (tuple-set index attr-value equal-p-fn less-p-fn)
  ;; Union appropriate tuple numbers into tuple-set.
  (let* ((root-pos (index-root-pos index))
         (root (when root-pos (index-node index root-pos))))
    (when root
      ;; Index has at least one node, so get tuple-numbers of the attr
      ;; that has the greatest value less than or equal to attr-value.
      (if (index-node-leaf-p root)
        ;; If Index has only one leaf node, compare attr-values.
        (let ((root-value (index-node-attr-value root)))
          (when (funcall less-p-fn root-value attr-value)
            (tuple-set-union-list tuple-set (index-node-tuple-list root))))
        ;; Otherwise, find matching leaf node or appropriate parent.
        (let* ((node-pos (index-find index attr-value equal-p-fn less-p-fn))
               (node (index-node index node-pos))
               leaf-pos)
          (if (index-node-leaf-p node)
            ;; If matching attribute-value leaf node found, 
            ;;    union in its predecessor's tuple-list.
            (when (setf leaf-pos (index-node-pred-leaf-pos node))
              (tuple-set-union-list tuple-set (index-node-tuple-list
                                               (index-node index leaf-pos))))
            ;; Otherwise, get leaf that is next greatest (if any) and use its predecessor.
            (let (pred-pos)
              (if (setf leaf-pos (index-node-descendant-pos node attr-value
                                                            equal-p-fn less-p-fn))
                ;; If next greatest leaf node exists, use its predecessor (if any).
                (setf pred-pos (index-node-pred-leaf-pos (index-node index leaf-pos)))
                ;; Otherwise, use max-attr-leaf.
                (setf pred-pos (index-max-attr-leaf-pos index)))
              (when pred-pos
                (tuple-set-union-list tuple-set (index-node-tuple-list
                                                 (index-node index pred-pos)))))))))
    tuple-set))

(defun index-query-succ (tuple-set index attr-value equal-p-fn less-p-fn)
  ;; Union appropriate tuple numbers into tuple-set.
  (let* ((root-pos (index-root-pos index))
         (root (when root-pos (index-node index root-pos))))
    (when root
      ;; Index has at least one node, so get tuple-numbers of the attr
      ;; that has the smallest value greater than or equal to attr-value.
      (if (index-node-leaf-p root)
        ;; If Index has only one leaf node, compare attr-values.
        (let ((root-value (index-node-attr-value root)))
          (when (attr-gt-p root-value attr-value equal-p-fn less-p-fn)
            (tuple-set-union-list tuple-set (index-node-tuple-list root))))
        ;; Otherwise, find matching leaf node or appropriate parent.
        (let* ((node-pos (index-find index attr-value equal-p-fn less-p-fn))
               (node (index-node index node-pos))
               leaf-pos)
          (if (index-node-leaf-p node)
            ;; If matching attribute-value leaf node found, union in its tuple-list.
            (when (setf leaf-pos (index-node-succ-leaf-pos node))
              (tuple-set-union-list
               tuple-set (index-node-tuple-list (index-node index leaf-pos))))
            ;; Otherwise, use leaf that is next greatest (if any).
            (when (setf leaf-pos (index-node-descendant-pos node attr-value
                                                            equal-p-fn less-p-fn))
              (tuple-set-union-list
               tuple-set (index-node-tuple-list (index-node index leaf-pos))))))))
    tuple-set))

(defun index-query-floor (tuple-set index attr-value equal-p-fn less-p-fn)
  ;; Union appropriate tuple numbers into tuple-set.
  (let* ((root-pos (index-root-pos index))
         (root (when root-pos (index-node index root-pos))))
    (when root
      ;; Index has at least one node, so get tuple-numbers of the attr
      ;; that has the greatest value less than or equal to attr-value.
      (if (index-node-leaf-p root)
        ;; If Index has only one leaf node, compare attr-values.
        (let ((root-value (index-node-attr-value root)))
          (when (attr-le-p root-value attr-value equal-p-fn less-p-fn)
            (tuple-set-union-list tuple-set (index-node-tuple-list root))))
        ;; Otherwise, find matching leaf node or appropriate parent.
        (let* ((node-pos (index-find index attr-value equal-p-fn less-p-fn))
               (node (index-node index node-pos)))
          (if (index-node-leaf-p node)
            ;; If matching attribute-value leaf node found, union in its tuple-list.
            (tuple-set-union-list tuple-set (index-node-tuple-list node))
            ;; Otherwise, get leaf that is next greatest (if any) and use its predecessor.
            (let ((leaf-pos (index-node-descendant-pos node attr-value
                                                       equal-p-fn less-p-fn))
                  pred-pos)
              (if leaf-pos
                ;; If next greatest leaf node exists, use its predecessor (if any).
                (setf pred-pos (index-node-pred-leaf-pos (index-node index leaf-pos)))
                ;; Otherwise, use max-attr-leaf.
                (setf pred-pos (index-max-attr-leaf-pos index)))
              (when pred-pos
                (tuple-set-union-list tuple-set (index-node-tuple-list
                                                 (index-node index pred-pos)))))))))
    tuple-set))

(defun index-query-ceiling (tuple-set index attr-value equal-p-fn less-p-fn)
  ;; Union appropriate tuple numbers into tuple-set.
  (let* ((root-pos (index-root-pos index))
         (root (when root-pos (index-node index root-pos))))
    (when root
      ;; Index has at least one node, so get tuple-numbers of the attr
      ;; that has the smallest value greater than or equal to attr-value.
      (if (index-node-leaf-p root)
        ;; If Index has only one leaf node, compare attr-values.
        (let ((root-value (index-node-attr-value root)))
          (unless (funcall less-p-fn root-value attr-value)
            (tuple-set-union-list tuple-set (index-node-tuple-list root))))
        ;; Otherwise, find matching leaf node or appropriate parent.
        (let* ((node-pos (index-find index attr-value equal-p-fn less-p-fn))
               (node (index-node index node-pos)))
          (if (index-node-leaf-p node)
            ;; If matching attribute-value leaf node found, union in its tuple-list.
            (tuple-set-union-list tuple-set (index-node-tuple-list node))
            ;; Otherwise, use leaf that is next greatest (if any).
            (let ((leaf-pos (index-node-descendant-pos node attr-value
                                                       equal-p-fn less-p-fn)))
              (when leaf-pos
                (tuple-set-union-list
                 tuple-set (index-node-tuple-list (index-node index leaf-pos)))))))))
    tuple-set))

(defmacro def-ind-rangefn (theta between-p a1-closed-p a2-closed-p)
  "Generate index query function for specified range theta clause operator."
  (let* ((pkg (find-package "RTM"))
         (ind-query-fn (intern (concatenate 'string "INDEX-QUERY-" theta) pkg))
         (attr-comp-mc (intern (concatenate 'string "ATTR-" theta "-P") pkg)))
    `(progn
       (defun ,ind-query-fn (tuple-set index attr-value-1 attr-value-2
                                       equal-p-fn less-p-fn)
         ;; Union appropriate tuple numbers into tuple-set.
         (let* ((root-pos (index-root-pos index))
                (root (when root-pos (index-node index root-pos))))
           (when root
             ;; Index has at least one node.
             (if (index-node-leaf-p root)
               ;; If Index has only one leaf node, compare attr-values.
               (let ((root-value (index-node-attr-value root)))
                 (when (,attr-comp-mc root-value attr-value-1 attr-value-2
                                      equal-p-fn less-p-fn)
                   (tuple-set-union-list tuple-set (index-node-tuple-list root))))
               ;; Otherwise, find correct start and end node positions.
               (let* ((node-pos (index-find index attr-value-1 equal-p-fn less-p-fn))
                      (node (index-node index node-pos))
                      start-pos end-pos)
                 (if (index-node-leaf-p node)
                   ;; If matching attribute-value leaf node found, 
                   ;;    use it or its neighbor as start node.
                   (setf start-pos ,(if a1-closed-p
                                      `node-pos
                                      (if between-p
                                        `(index-node-succ-leaf-pos node)
                                        `(index-node-pred-leaf-pos node))))
                   ;; Otherwise, get leaf that is next greatest and use it 
                   ;;    or its neighbor as start node.
                   (let ((leaf-pos (index-node-descendant-pos
                                    node attr-value-1 equal-p-fn less-p-fn)))
                     ,(if between-p
                        `(setf start-pos leaf-pos)
                        `(when leaf-pos
                           (setf start-pos (index-node-pred-leaf-pos
                                            (index-node index leaf-pos)))))))
                 ;; Find matching leaf node or appropriate parent for attr-value-2.
                 (setf node-pos (index-find index attr-value-2 equal-p-fn less-p-fn)
                       node (index-node index node-pos))
                 (if (index-node-leaf-p node)
                   ;; If matching attribute-value leaf node found, 
                   ;;    use it or its neighbor as end node.
                   (setf end-pos ,(if a2-closed-p
                                    `node-pos
                                    (if between-p
                                      `(index-node-pred-leaf-pos node)
                                      `(index-node-succ-leaf-pos node))))
                   ;; Otherwise, get leaf that is next greatest and use it 
                   ;;    or its neighbor as end node.
                   (let ((leaf-pos (index-node-descendant-pos
                                    node attr-value-2 equal-p-fn less-p-fn)))
                     ,(if between-p
                        `(if leaf-pos
                           ;; If next greatest leaf node exists, use its predecessor as end node.
                           (setf end-pos (index-node-pred-leaf-pos
                                          (index-node index leaf-pos)))
                           ;; Otherwise, use max-attr-leaf as end node.
                           (setf end-pos (index-max-attr-leaf-pos index)))
                        `(setf end-pos leaf-pos))))
                 ,(if between-p
                    ;; If both start-pos and end-pos are in index, 
                    ;;    and end is not predecessor of start, union in 
                    ;;    tuple-lists along thread from start-pos to end-pos.
                    `(when (and start-pos end-pos
                                (not (eql (index-node-pred-leaf-pos
                                           (index-node index start-pos))
                                          end-pos)))
                       (index-union-interval-thread tuple-set index start-pos end-pos))
                    `(progn
                       ;; If start-pos is in index, union in tuple-lists along 
                       ;;    thread from min-pos to start-pos.
                       (when start-pos 
                         (index-union-interval-thread tuple-set index nil start-pos))
                       ;; If end-pos is in index, union in tuple-lists along 
                       ;;    thread from end-pos to max-pos.
                       (when end-pos 
                         (index-union-interval-thread tuple-set index end-pos nil)))))))
           tuple-set)))))


(def-ind-rangefn "GE-LE"  t   t   t )
(def-ind-rangefn "GT-LE"  t  nil  t )
(def-ind-rangefn "GE-LT"  t   t  nil)
(def-ind-rangefn "GT-LT"  t  nil nil)

(def-ind-rangefn "LE-GE" nil  t   t )
(def-ind-rangefn "LT-GE" nil nil  t )
(def-ind-rangefn "LE-GT" nil  t  nil)
(def-ind-rangefn "LT-GT" nil nil nil)

