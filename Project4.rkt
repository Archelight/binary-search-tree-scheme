;Nikita Romanovsky
;4/30/2020
;Professor: Qi Wang

;The program represents a Binary Search Tree. It implements different functionalities of a BST such as inserting an element into the BST, searching for an element in the BST,
; getting the root of the BST, traversing the BST in preorder, inorder, and postorder, checking if the BST is empty, and getting the left/right subtrees of the BST. Finaly,
; the program features three different consturctors for a BST, able to create an empty BST, a BST with a root element, and a BST with a filled left/right subtrees

;creates an empty binary search tree
;return: an empty list
(define (createEmptyBST)
    '())

;creates a binary search tree with a root and empty left/right subtrees
;param: element, an element to be placed as the root of the tree
(define (createRootBST element)
    (list element '() '()))

;creates a binary search tree with a root and left/right subtrees
;param: element, an element to be placed as the root of the tree
;oaram: leftSubtree, a BST to be placed as the left subtree of the new BST
;param rightSubtree, a BST to be places as the right subtree of the new BSt
;returns a BST with a root, a leftsubtree, and a rightsubtree
(define (createFullBST element leftSubtree rightSubtree)
  (if(and (and (not(NULL? element)) (LIST? leftSubtree)) (LIST? rightSubtree)) ;checks if the root element is null and if the left/right subtrees are not lists, if they are return #f
   (cond
     ((or (NULL? leftSubtree) (NULL? rightSubtree))  ;checks if the left subtree or right subrees are empty, if one of them is, enter condition
      (cond
        ((and (NULL? (getRoot leftSubtree)) (not(NULL? (getRoot rightSubtree)))) ;if the left subtree is null but the right one is not, enter condition
           (if(> (car rightSubtree) element) ;if the root of the right sub tree is more than the element, create the BST, otherwise return #f
              (list element leftSubtree rightSubtree) #f))
        ((and (NULL? (getRoot rightSubtree)) (not(NULL? (getRoot leftSubtree)))) ;if the right subtree is null but the left one is not, enter condition
           (if(< (car leftSubtree) element) ;if the root of the left sub tree is less than the element, create the BST, otherwise return #f
              (list element leftSubtree rightSubtree) #f)) 
        (else (list element '() '())))) ;if both elements are null, make a list with left/right subtrees empty
     ((and (< (car leftSubtree) element) (> (car rightSubtree) element)) ;if both subtrees are not empty, and the root of the left subtree is less than the element, while the root of the right subtree is greater than the element
      (list element leftSubtree rightSubtree)) ;create the BST
     (else #f))#f)) ;if one of the input parameters is invalid, return #f
     
;returns the root of the binary search tree
;param: BST, a Binary search tree
;returns the root of the tree
(define (getRoot BST)
  (if(NULL? BST) ;checks if the root element of the BST is null, making sure the BST is not empty
     '() (car BST))) ;If the BST is not empty, return the first element in the list (root)

;param: BST, a binary search tree
;returns the left subtree of the binary search tree
(define (getLeftSubtree BST)
  (if(NULL?(car(cdr BST))) ;checks that the left subtree is not empty
     '() (car(cdr BST)))) ;if the left subtree is empty, return an empty list, else return the left subtree

;param: BST, a binary search tree
;returns the right subtree of the binary search tree
(define (getRightSubtree BST)
  (if(NULL?(car(cdr(cdr BST)))) ;checks that the left subtree is not empty
     '() (car(cdr(cdr BST))))) ;if the right subtree is empty, return an empty list, else return the right subtree

;param: BST, a binary search tree
;checks if the binary search tree is empty
;returns #t or #f depending on if the BST is empty
(define (checkEmpty BST)
  (null? BST))

;finds a specific element in the binary search tree and returns #t if it is found, #f if not
;param: BST, a binary search tree
;param: element, element to be found in the BST
;returns #t or #f depending on if the element was found
(define (findElement BST element)
  (cond
    ((checkEmpty BST) #f) ;termination condition. If the end of the BST was reached, aka the subtree is null, the element was not found so return #f
    ((= element (getRoot BST)) #t) ;termination condition. If the element to be searched for is equal to the root of a subtree, meaning the element was found in the BST, return #t
    ((< element (getRoot BST)) ;recursion, If the element to be searched for is less than the root of the BST, search the left subtree for the element
     (findElement (getLeftSubtree BST) element)) 
    ((> element (getRoot BST)) ;recursion, If the element to be searched for is greater than the root of the BST, search the right subtree for the element
     (findElement (getRightSubtree BST) element))))

;inserts an element in the correct position inside a binary search tree.
;if the element is already in the BST, return the unchanged BST
;param: BST, a binary search tree
;param: element, element to be inserted into the BST
;returns a new BST with the inserted element
(define (insertElement BST element)
  (cond
    ((checkEmpty BST) ;termination condition, If an empty position is found in the BST at a proper position, make a new subtree out of the new element
    (createFullBST element '() '()))
    ((< element (getRoot BST)) ;if the element to be inserted is lesser than the root of the BST, create a new BST with a new left subtree
     (createFullBST ;create a new BST out of the root of the current BST, a new left subtree holding the new element ,and the right subtree of the current BST 
      (getRoot BST)
      (insertElement (getLeftSubtree BST) element) ;recursion, insert the new element into the left subtree of the BST
      (getRightSubtree BST)))
    ((> element (getRoot BST)) ;if the element to be inserted is greater than the root of the BST, create a new BST with a new right subtree
     (createFullBST ;create a new BST out of the root of the current BST, a new right subtree holding the new element ,and the left subtree of the current BST
      (getRoot BST)
      (getLeftSubtree BST)
      (insertElement (getRightSubtree BST) element)))  ;recursion, insert the new element into the right subtree of the BST
    ((= element (getRoot BST)) BST))) ;if the root is equal to the new element, the element is already in the BST, return the BST

;traverses the BST inorder, returns a list of the BST elements traversed in inorder
;param: BST, a binary search tree
;returns a a list of elements representing the BST after inorder traversal
(define (traverseInorder BST) ;appends the left subtree, the root, and the right subtree of each subtree
  (cond
    ((checkEmpty BST) '()) ;termination condition, if the BST is empty, the end of the BST has been reached ,return null since function append only appends lists
    (else (append (traverseInorder (getLeftSubtree BST)) ;recursion, traverses the left subtree
                  (list(getRoot BST))
                  (traverseInorder (getRightSubtree BST)))))) ;recursion, traverses the right subtree

;traverses the BST in postorder, returns a list of the BST elements traversed in postorder
;param: BST, a binary search tree
;returns a a list of elements representing the BST after inorder traversal
(define (traversePostorder BST) ;appends the left subtree, and the right subtree, and the root of each subtree
  (cond
    ((checkEmpty BST) '()) ;termination condition, if the BST is empty, the end of the BST has been reached ,return null since function append only appends lists
    (else (append (traversePostorder (getLeftSubtree BST)) ;recursion, traverses the left subtree
                  (traversePostorder (getRightSubtree BST)) ;recursion, traverses the right subtree
                  (list(getRoot BST))))))

;traverses the BST in preorder, returns a list of the BST elements travered in preorder
;param: BST, a binary search tree
;returns a a list of elements representing the BST after inorder traversal
(define (traversePreorder BST) ;appends the root, the left subtree, and the right subtree of each subtree
  (cond
    ((checkEmpty BST) '()) ;termination condition, if the BST is empty, the end of the BST has been reached ,return null since function append only appends lists
    (else (append (list(getRoot BST)) 
                  (traversePreorder (getLeftSubtree BST)) ;recursion, traverses the left subtree
                  (traversePreorder (getRightSubtree BST)))))) ;recursion, traverses the right subtree


;Test Cases:

;Function createEmptyBST:
;Test Case 1: (createEmptyBST)
;Result: ()
;Test Case 2: (createEmptyBST)
;Result: ()

;Function createRootBST:
;Test Case 1: (createRootBST 1)
;Result: (1 () ())
;Test Case 2: (createRootBST 2)
;Result: (2 () ())

;Function createFullBST:
;Test Case 1: (createFullBST 1 (list 0 '() '()) (list 3 '() '()))
;Result: (1 (0 () ()) (3 () ()))
;Test Case 2: (createFullBST 6 (list 6 '() '()) (list 5 '() '()))
;Result: #f

;Function getRoot:
;Test Case 1: (getRoot (list 2 (list 1 '() '()) (list 3 '() '())))
;Result: 2 
;Test Case 2: (getRoot '())
;Result: '()

;Function getLeftSubtree:
;Test Case 1: (getLeftSubtree (list 4(list 2 '() '()) (list 5 '() '())))
;Result: (2 () ())
;Test Case 2: (getLeftSubtree (list 6(list 5 '() '()) (list 8 '() '())))
;Result: (5 () ())

;Function getRightSubtree:
;Test Case 1: (getRightSubtree (list 4(list 2 '() '()) (list 5 '() '())))
;Result: (5 () ())
;Test Case 2: (getRightSubtree (list 6(list 5 '() '()) (list 8 '() '())))
;Result: (8 () ())

;Function checkEmpty:
;Test Case 1: (checkEmpty '())
;Result: #t
;Test Case 2: (checkEmpty (list 4(list 2 '() '()) (list 5 '() '())))
;Result: #f

;Function findElement:
;Text Case 1: (findElement (list 4 (list 3 '() '()) (list 6 '() '())) 3)
;Result: #t
;Text Case 2: (findElement (list 5 '() '()) 3)
;Result: #f

;Function insertElement:
;Text Case 1: (insertElement (list 5 '() '()) 3)
;Result: (5 (3 () ()) ()) 
;Text Case 2: (insertElement (list 5 '() '()) 6)
;Result: (5 () (6 () ()))

;Function traverseInorder:
;Test Case 1: (traverseInorder (list 5 (list 4 '() '()) (list 6 '() '())))
;Result: (4 5 6)
;Test Case 2: (traverseInorder (list 5 '() '()))
;Result: (5)

;Function traversePostorder:
;Test Case 1: (traversePostorder (list 5 (list 4 '() '()) (list 6 '() '())))
;Result: (4 6 5)
;Test Case 2: (traversePostorder (list 5 '() '()))
;Result: (5)

;Function traversePreorder:
;Test Case 1: (traversePreorder (list 5 (list 4 '() '()) (list 6 '() '())))
;Result: (5 4 6)
;Test Case 2: (traversePreorder (list 5 '() '()))
;Result: (5)

;Entire Program Test
;Test Case 1:

;Command 1: (checkEmpty (createEmptyBST))
;Result: #t
;Command 2: (findElement (createRootBST 10) 10)
;Result: #t
;Command 3: (insertElement (createRootBST 10) 5)
;Result: (10 (5 () ()) ())
;Command 4: (Define BST (createFullBST 14 (list 3 '() '()) (list 21 '() '())))
;Command 5: (traverseInorder BST)
;Result: (3 14 21)
;Command 6: (traversepostorder BST)
;Result: (3 21 14)
;Command 7: (traversepreorder BST)
;Result: (14 3 21)
;Command 8: (getRoot BST)
;Result: 14
;Command 9: (getLeftSubtree BST)
;Result: (3 () ())
;Command 10: (getRightSubtree BST)
;Result: (21 () ())

;Test Case 2:

;Command 1: (checkEmpty (createEmptyBST))
;Result: #t
;Command 2: (findElement (insertElement (createRootBST 10) 5) 5)
;Result: #t
;Command 3: (Define BST (createFullBST 9 (list 3 '() '()) (list 11 '() '())))
;Command 4: (traverseInorder BST)
;Result: (3 9 11)
;Command 5: (traversepostorder BST)
;Result: (3 11 9)
;Command 6: (traversepreorder BST)
;Result: (9 3 11)
;Command 7: (Define BST (createFullBST 10 (list 2 '() '()) (list 11 '() '())))
;Command 8: (getRoot BST)
;Result: 10
;Command 9: (getLeftSubtree BST)
;Result: (2 () ())
;Command 10: (getRightSubtree BST)
;Result: (11 () ())
