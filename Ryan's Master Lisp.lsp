;; AutoCAD lisp set
;; Compiled by Ryan Whitworth


;; Command Name               (Command to execute)

;; 3D Stretch                 (3DS)
;; Purge, Audit, Zoom, Save   (PP)
;; Viewport Outline           (VPO for one viewport; VPOL for all)
;; Additive Dimensioner       (AD)
;; Default View-setter        (DEF to select view boundary, DE to jump back to view)
;; Millimeter Draw            (MM)
;; LineMid		                (LM)
;; XL Master                  (SQ for diagonal up and to the left; AW for diagonal up and to the right)
;; Reload Lisps               (RLL) [requires a file location, in a precise format]


;; 3D Stretch (3DS)
;; By ______________



;; Purge, Audit, Zoom, Save (PP)
;; By Ryan Whitworth

(defun c:OP ()
  ;; Loop to ensure all items are purged
  (while (not (eq (command "_.-PURGE" "ALL" "*" "N") nil))
    ;; Execute the purge command
    (command "_.-PURGE" "ALL" "*" "N")
  )
  
  (princ "\nPurge completed.")
  (princ)
)

(defun c:PP ()
  ;; Call the OP function to purge all items
  (c:OP)
  
  ;; Audit the drawing and fix errors
  (command "_.AUDIT" "Y")
  
  ;; Zoom to extents
  (command "_.ZOOM" "E")
  
  ;; Get the current drawing file name
  (setq filename (getvar "DWGNAME"))
  
  ;; Get the file extension
  (setq file-ext (strcase (substr filename (- (strlen filename) 2) 3)))
  
  ;; Debugging output
  (princ (strcat "\nFilename: " filename))
  (princ (strcat "\nFile extension: " file-ext))
  
  ;; Check if the file is a DXF or DWG and save accordingly
  (cond
    ((= file-ext "DXF") (c:2007dxf))
    ((= file-ext "DWG") (command "_.QSAVE"))
    (T (princ "\nThe file is neither a DXF nor a DWG."))
  )
  
  (princ "\nPurge, Audit, Zoom, and Save completed.")
  (princ)
)



;; Viewport Outline (VPO | VPOL)
;; By Lee Mac, www.lee-mac.com

(defun c:vpo ( / *error* sel )

    (defun *error* ( msg )
        (LM:endundo (LM:acdoc))
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

    (LM:startundo (LM:acdoc))
    (cond
        (   (/= 1 (getvar 'cvport))
            (princ "\nCommand not available in Modelspace.")
        )
        (   (setq sel (LM:ssget "\nSelect viewport: " '("_+.:E:S" ((0 . "VIEWPORT")))))
            (vpo:main (ssname sel 0))
        )
    )
    (LM:endundo (LM:acdoc))
    (princ)
)

;;----------------------------------------------------------------------;;
;;  VPOL - Outline all viewports in the active Paperspace layout        ;;
;;----------------------------------------------------------------------;;

(defun c:vpol ( / *error* idx sel )

    (defun *error* ( msg )
        (LM:endundo (LM:acdoc))
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

    (cond
        (   (/= 1 (getvar 'cvport))
            (princ "\nCommand not available in Modelspace.")
        )
        (   (setq sel (ssget "_X" (list '(0 . "VIEWPORT") '(-4 . "<>") '(69 . 1) (cons 410 (getvar 'ctab)))))
            (LM:startundo (LM:acdoc))
            (repeat (setq idx (sslength sel))
                (vpo:main (ssname sel (setq idx (1- idx))))
            )
            (LM:endundo (LM:acdoc))
        )
        (   (princ "\nNo viewports were found in the active layout."))
    )
    (princ)
)

;;----------------------------------------------------------------------;;
;;  VPOA - Outline all viewports in all Paperspace layouts              ;;
;;----------------------------------------------------------------------;;

(defun c:vpoa ( / *error* idx sel )

    (defun *error* ( msg )
        (LM:endundo (LM:acdoc))
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

    (cond
        (   (setq sel (ssget "_X" '((0 . "VIEWPORT") (-4 . "<>") (69 . 1) (410 . "~Model"))))
            (LM:startundo (LM:acdoc))
            (repeat (setq idx (sslength sel))
                (vpo:main (ssname sel (setq idx (1- idx))))
            )
            (LM:endundo (LM:acdoc))
        )
        (   (princ "\nNo viewports were found in any Paperspace layouts."))
    )
    (princ)
)

;;----------------------------------------------------------------------;;

(defun vpo:main ( vpt / cen dpr ent lst ltp ocs ofe off tmp vpe )

    (setq

;;----------------------------------------------------------------------;;
;;                          Program Parameters                          ;;
;;----------------------------------------------------------------------;;

        ;; Optional Interior Offset
        ;; Set this parameter to nil or 0.0 for no offset
        off 0.0

        ;; Default Polyline Properties
        ;; Omitted properties will use current settings when the program is run
        dpr
       '(
            (006 . "BYLAYER")   ;; Linetype (must be loaded)
           ;(008 . "VPOutline") ;; Layer (automatically created if not present in drawing)
            (039 . 0.0)         ;; Thickness
            (048 . 1.0)         ;; Linetype Scale
            (062 . 256)         ;; Colour (0 = ByBlock, 256 = ByLayer)
            (370 . -1)          ;; Lineweight (-1 = ByLayer, -2 = ByBlock, -3 = Default, 0.3 = 30 etc.)
        )
        
;;----------------------------------------------------------------------;;

    )
    
    (if (setq vpt (entget vpt)
              ent (cdr (assoc 340 vpt))
        )
        (setq lst (vpo:polyvertices ent))
        (setq cen (mapcar 'list (cdr (assoc 10 vpt))
                      (list
                          (/ (cdr (assoc 40 vpt)) 2.0)
                          (/ (cdr (assoc 41 vpt)) 2.0)
                      )
                  )
              lst (mapcar '(lambda ( a ) (cons (mapcar 'apply a cen) '(42 . 0.0))) '((- -) (+ -) (+ +) (- +)))
        )
    )
    (if (not (LM:listclockwise-p (mapcar 'car lst)))
        (setq lst (reverse (mapcar '(lambda ( a b ) (cons (car a) (cons 42 (- (cddr b))))) lst (cons (last lst) lst))))
    )
    (if (and (numberp off) (not (equal 0.0 off 1e-8)))
        (cond
            (   (null
                    (setq tmp
                        (entmakex
                            (append
                                (list
                                   '(000 . "LWPOLYLINE")
                                   '(100 . "AcDbEntity")
                                   '(100 . "AcDbPolyline")
                                    (cons 90 (length lst))
                                   '(070 . 1)
                                )
                                (apply 'append (mapcar '(lambda ( x ) (list (cons 10 (car x)) (cdr x))) lst))
                            )
                        )
                    )
                )
                (princ "\nUnable to generate Paperspace outline for offset.")
            )
            (   (vl-catch-all-error-p (setq ofe (vl-catch-all-apply 'vlax-invoke (list (vlax-ename->vla-object tmp) 'offset off))))
                (princ (strcat "\nViewport dimensions too small to offset outline by " (rtos off) " units."))
                (entdel tmp)
            )
            (   (setq ofe (vlax-vla-object->ename (car ofe))
                      lst (vpo:polyvertices ofe)
                )
                (entdel ofe)
                (entdel tmp)
            )
    	)
    )
    (setq vpe (cdr (assoc -1 vpt))
          ocs (cdr (assoc 16 vpt))
    )
    (entmakex
        (append
            (list
               '(000 . "LWPOLYLINE")
               '(100 . "AcDbEntity")
               '(100 . "AcDbPolyline")
                (cons 90 (length lst))
               '(070 . 1)
               '(410 . "Model")
            )
            (if (and (setq ltp (assoc 6 dpr)) (not (tblsearch "ltype" (cdr ltp))))
                (progn
                    (princ  (strcat "\n\"" (cdr ltp) "\" linetype not loaded - linetype set to \"ByLayer\"."))
                    (subst '(6 . "BYLAYER") ltp dpr)
                )
                dpr
            )
            (apply 'append (mapcar '(lambda ( x ) (list (cons 10 (trans (pcs2wcs (car x) vpe) 0 ocs)) (cdr x))) lst))
            (list (cons 210 ocs))
        )
    )
)

;;----------------------------------------------------------------------;;

(defun vpo:polyvertices ( ent )
    (apply '(lambda ( foo bar ) (foo bar))
        (if (= "LWPOLYLINE" (cdr (assoc 0 (entget ent))))
            (list
                (lambda ( enx )
                    (if (setq enx (member (assoc 10 enx) enx))
                        (cons (cons  (cdr (assoc 10 enx)) (assoc 42 enx)) (foo (cdr enx)))
                    )
                )
                (entget ent)
            )
            (list
                (lambda ( ent / enx )
                    (if (= "VERTEX" (cdr (assoc 0 (setq enx (entget ent)))))
                        (cons (cons (cdr (assoc 10 enx)) (assoc 42 enx)) (foo (entnext ent)))
                    )
            	)
                (entnext ent)
            )
        )
    )
)

;;----------------------------------------------------------------------;;

;; List Clockwise-p  -  Lee Mac
;; Returns T if the point list is clockwise oriented

(defun LM:listclockwise-p ( lst )
    (minusp
        (apply '+
            (mapcar
                (function
                    (lambda ( a b )
                        (- (* (car b) (cadr a)) (* (car a) (cadr b)))
                    )
                )
                lst (cons (last lst) lst)
            )
        )
    )
)

;; ssget  -  Lee Mac
;; A wrapper for the ssget function to permit the use of a custom selection prompt
;; msg - [str] selection prompt
;; arg - [lst] list of ssget arguments

(defun LM:ssget ( msg arg / sel )
    (princ msg)
    (setvar 'nomutt 1)
    (setq sel (vl-catch-all-apply 'ssget arg))
    (setvar 'nomutt 0)
    (if (not (vl-catch-all-error-p sel)) sel)
)

;; PCS2WCS (gile)
;; Translates a PCS point to WCS based on the supplied Viewport
;; (PCS2WCS pt vp) is the same as (trans (trans pt 3 2) 2 0) when vp is active
;; pnt : PCS point
;; ent : Viewport ename

(defun PCS2WCS ( pnt ent / ang enx mat nor scl )
    (setq pnt (trans pnt 0 0)
          enx (entget ent)
          ang (- (cdr (assoc 51 enx)))
          nor (cdr (assoc 16 enx))
          scl (/ (cdr (assoc 45 enx)) (cdr (assoc 41 enx)))
          mat (mxm
                  (mapcar (function (lambda ( v ) (trans v 0 nor t)))
                     '(   (1.0 0.0 0.0)
                          (0.0 1.0 0.0)
                          (0.0 0.0 1.0)
                      )
                  )
                  (list
                      (list (cos ang) (- (sin ang)) 0.0)
                      (list (sin ang)    (cos ang)  0.0)
                     '(0.0 0.0 1.0)
                  )
              )
    )
    (mapcar '+
        (mxv mat
            (mapcar '+
                (vxs pnt scl)
                (vxs (cdr (assoc 10 enx)) (- scl))
                (cdr (assoc 12 enx))
            )
        )
        (cdr (assoc 17 enx))
    )
)

;; Matrix Transpose  -  Doug Wilson
;; Args: m - nxn matrix

(defun trp ( m )
    (apply 'mapcar (cons 'list m))
)

;; Matrix x Matrix  -  Vladimir Nesterovsky
;; Args: m,n - nxn matrices

(defun mxm ( m n )
    ((lambda ( a ) (mapcar '(lambda ( r ) (mxv a r)) m)) (trp n))
)

;; Matrix x Vector  -  Vladimir Nesterovsky
;; Args: m - nxn matrix, v - vector in R^n

(defun mxv ( m v )
    (mapcar '(lambda ( r ) (apply '+ (mapcar '* r v))) m)
)

;; Vector x Scalar  -  Lee Mac
;; Args: v - vector in R^n, s - real scalar

(defun vxs ( v s )
    (mapcar '(lambda ( n ) (* n s)) v)
)

;; Start Undo  -  Lee Mac
;; Opens an Undo Group.

(defun LM:startundo ( doc )
    (LM:endundo doc)
    (vla-startundomark doc)
)

;; End Undo  -  Lee Mac
;; Closes an Undo Group.

(defun LM:endundo ( doc )
    (while (= 8 (logand 8 (getvar 'undoctl)))
        (vla-endundomark doc)
    )
)

;; Active Document  -  Lee Mac
;; Returns the VLA Active Document Object

(defun LM:acdoc nil
    (eval (list 'defun 'LM:acdoc 'nil (vla-get-activedocument (vlax-get-acad-object))))
    (LM:acdoc)
)

;;----------------------------------------------------------------------;;

(princ
    (strcat
        "\n:: VPOutline.lsp | Version 1.3 | \\U+00A9 Lee Mac "
        ((lambda ( y ) (if (= y (menucmd "m=$(edtime,0,yyyy)")) y (strcat y "-" (menucmd "m=$(edtime,0,yyyy)")))) "2015")
        " www.lee-mac.com ::"
        "\n:: \"vpo\"  - Outline single viewport                ::"
        "\n:: \"vpol\" - Outline all viewports in active layout ::"
        "\n:: \"vpoa\" - Outline all viewports in all layouts   ::"
    )
)
(princ)





;; Additive Dimensioner (AD)
;; By Ryan Whitworth
;; Measures a set of lines, polylines, and/or arcs, and draws a line that is the sum of their lengths.
;; Especially useful for unrolling, like with a curving range hood.
;; Could be modified to return the total as a number, but arc lengths are rarely round numbers; this allows for greater precision.

(defun C:AD ()
  ;; Prompt the user to select objects
  (setq selSet (ssget '((0 . "LINE,ARC,LWPOLYLINE,POLYLINE"))))
  
  ;; Initialize the total length variable
  (setq totalLength 0.0)
  
  ;; Iterate through the selected objects
  (if selSet
    (progn
      (setq count (sslength selSet))
      (setq i 0)
      (while (< i count)
        (setq ent (ssname selSet i))
        (setq entData (entget ent))
        (setq entType (cdr (assoc 0 entData)))
        
        ;; Calculate the length based on the entity type
        (cond
          ((equal entType "LINE")
           (setq startPt (cdr (assoc 10 entData)))
           (setq endPt (cdr (assoc 11 entData)))
           (setq length (distance startPt endPt))
           (setq totalLength (+ totalLength length))
          )
          ((equal entType "ARC")
           (setq radius (cdr (assoc 40 entData)))
           (setq startAngle (cdr (assoc 50 entData)))
           (setq endAngle (cdr (assoc 51 entData)))
           (setq length (* radius (abs (- endAngle startAngle))))
           (setq totalLength (+ totalLength length))
          )
          ((or (equal entType "LWPOLYLINE") (equal entType "POLYLINE"))
           (setq length (vlax-curve-getDistAtParam ent (vlax-curve-getEndParam ent)))
           (setq totalLength (+ totalLength length))
          )
        )
        
        (setq i (1+ i))
      )
    )
  )

  ;; Debugging: Print total length
  (princ (strcat "\nTotal Length Calculated: " (rtos totalLength 2 4)))

  ;; Prompt the user to specify a point to start the horizontal line
  (setq startPoint (getpoint "\nSpecify the start point for the horizontal line: "))

  ;; Debugging: Print start point
  (princ (strcat "\nStart Point: " (rtos (car startPoint) 2 4) ", " (rtos (cadr startPoint) 2 4)))
  
  ;; Calculate the end point of the horizontal line
  (setq endPoint (polar startPoint 0.0 totalLength))

  ;; Draw the horizontal line
  (command "_.LINE" startPoint endPoint "")

  ;; Print the total length
  (princ (strcat "\nTotal length of selected lines, arcs, and polylines: " (rtos totalLength 2 4)))

  ;; Clean up
  (princ)
)



;; Default View-setter (DEF | DE)
;; By Ryan Whitworth
;; Defines a default view with command DEF, and saves it as text within the file.
;; Zooms to those boundaries with command DE

;;modify the location that text is saved to just above the asterisked notes below (2 places)


(defun strsplit (str delim / start end result)
  (setq start 0)
  (while (setq end (vl-string-search delim str start))
    (setq result (cons (substr str (+ start 1) (- end start)) result))
    (setq start (+ end (strlen delim)))
  )
  (reverse (cons (substr str (+ start 1)) result))
)

(defun C:DE ()
  (setq textLocation (list 0 0))
******CHANGE COORDINATES ABOVE IF YOU WANT THE TEXT SAVED SOMEWHERE ELSE.******
  (setq textEntity (ssget "X" (list (cons 0 "TEXT") (cons 10 textLocation))))
  (if textEntity
    (progn
      (setq textHandle (ssname textEntity 0))
      (setq textContent (cdr (assoc 1 (entget textHandle))))
      (setq coords (mapcar 'atof (strsplit textContent ",")))
      (if (= (length coords) 4)
        (progn
          (command "_.ZOOM" "_W" (list (nth 0 coords) (nth 1 coords)) (list (nth 2 coords) (nth 3 coords)))
          (princ "\nZoom window set to specified coordinates.")
        )
        (princ "\nError: Invalid coordinate format in text.")
      )
    )
    (princ "\nText not found at specified location. Please enter 'DEF' to define the window.")
  )
  (princ)
)

(defun C:DEF ()
  (setq textLocation (list 0 0))
******CHANGE COORDINATES ABOVE IF YOU WANT THE TEXT SAVED SOMEWHERE ELSE.******
  (setq textEntity (ssget "X" (list (cons 0 "TEXT") (cons 10 textLocation))))
  (if textEntity
    (progn
      (setq textHandle (ssname textEntity 0))
      (entdel textHandle)
    )
  )
  (setq p1 (getpoint "\nSpecify first corner of zoom window: "))
  (setq p2 (getpoint "\nSpecify opposite corner of zoom window: "))
  (setq textContent (strcat (rtos (car p1) 2 2) "," (rtos (cadr p1) 2 2) "," (rtos (car p2) 2 2) "," (rtos (cadr p2) 2 2)))
  (entmake (list (cons 0 "TEXT") (cons 10 textLocation) (cons 1 textContent) (cons 40 0.2) (cons 7 "Standard")))
  (princ "\nZoom window coordinates saved.")
  (princ)
)



;; Millimeter Draw   (MM)
;; By Ryan Whitworth
;; Converts milimeters to inches and draws a line in the specified direction.

(defun c:MM ()
  ;; Prompt the user to click the start point
  (setq startPoint (getpoint "\nClick the start point: "))
  
  ;; Prompt the user to click to define the direction
  (setq endPoint (getpoint startPoint "\nClick to define direction: "))
  
  ;; Calculate the direction vector
  (setq directionVector (list (- (car endPoint) (car startPoint)) (- (cadr endPoint) (cadr startPoint)) (- (caddr endPoint) (caddr startPoint))))
  
  ;; Prompt the user to enter the line length in millimeters
  (setq lengthMM (getreal "\nEnter the line length in millimeters: "))
  
  ;; Convert the length from millimeters to inches
  (setq lengthInches (/ lengthMM 25.4))
  
  ;; Calculate the unit vector for the direction
  (setq magnitude (sqrt (+ (* (car directionVector) (car directionVector)) (* (cadr directionVector) (cadr directionVector)) (* (caddr directionVector) (caddr directionVector)))))
  (setq unitVector (list (/ (car directionVector) magnitude) (/ (cadr directionVector) magnitude) (/ (caddr directionVector) magnitude)))
  
  ;; Calculate the actual end point based on the length in inches
  (setq actualEndPoint (list (+ (car startPoint) (* (car unitVector) lengthInches)) (+ (cadr startPoint) (* (cadr unitVector) lengthInches)) (+ (caddr startPoint) (* (caddr unitVector) lengthInches))))
  
  ;; Draw the line
  (command "LINE" startPoint actualEndPoint "")
  
  ;; Print a message to confirm the line was drawn
  (princ (strcat "\nLine drawn from " (rtos (car startPoint) 2 2) ", " (rtos (cadr startPoint) 2 2) " to " (rtos (car actualEndPoint) 2 2) ", " (rtos (cadr actualEndPoint) 2 2)))
  
  ;; End the function
  (princ)
)



;; XL Master  (SQ | AW)
;; By Ryan Whitworth
;; Creates a diagonal xline at 135 degrees (up and to the left) or 45 degrees (up and to the right).

(defun c:SQ ()
  (command "._xline" "_ang" 135) ; 135 degrees for diagonal up and to the left
  (princ "\nDiagonal xline (up and to the left) created.")
  (princ)
) ; end of defun

(defun c:AW ()
  (command "._xline" "_ang" 45) ; 45 degrees for diagonal up and to the right
  (princ "\nDiagonal xline (up and to the right) created.")
  (princ)
) ; end of defun



;; Arc Length to Radius Calculator (ARL)
;; By Ryan Whitworth
;; This command calculates the radius of an arc based on the desired arc length and angle, then draws a circle with that radius.

(defun c:ARL ()
    ;; Prompt the user to click the start point
    (setq startPoint (getpoint "\nClick to place the center point: "))

    ;; Prompt the user to enter the arc length
    (setq ArcLength (getreal "\nEnter the desired Arc Length: "))

    ;; Prompt the user to enter the angle in degrees
    (setq Angle (getreal "\nEnter the desired angle in degrees: "))

    ;; Convert angle to radians
    (setq AngleRad (* Angle (/ pi 180.0)))

    ;; Calculate the radius
    (setq Radius (/ ArcLength AngleRad))

    ;; Draw the circle
    (command "_.circle" "_non" startPoint Radius )
    (princ (strcat "\nRadius calculated: " (rtos Radius 2 4)))
    (princ)
)


;; Stack Dimensions (c:StackDims)
;; By Ryan Whitworth

(defun c:SD (/ basePt secPts pt sidePt gap dimStyle dimData txtHeight dimDir stackDir v1 v2 cross i offsetPt)
  (princ "\nSelect base point: ")
  (setq basePt (getpoint))
  (princ "\nSelect secondary points (press Enter to finish): ")
  (setq secPts nil)
  (while (setq pt (getpoint "\nSelect secondary point: "))
    (setq secPts (cons pt secPts))
  )
  (setq secPts (reverse secPts))
  (princ "\nPick side to place dimensions: ")
  (setq sidePt (getpoint basePt "\nPick side: "))

  ;; Get current dimension style name
  (setq dimStyle (getvar "DIMSTYLE"))
  ;; Get dimension style table record
  (setq dimData (tblsearch "DIMSTYLE" dimStyle))
  ;; Get text height (group code 140)
  (setq txtHeight (cdr (assoc 140 dimData)))
  ;; Set gap to 2x text height
  (setq gap (* 2.0 txtHeight))
  (princ (strcat "\nGap set to " (rtos gap 2 2) " (2x text height)"))

  ;; Calculate direction from basePt to first secPt
  (setq dimDir (angle basePt (car secPts)))
  ;; Perpendicular direction for stacking
  (setq stackDir (+ dimDir (/ pi 2)))

  ;; Determine which side the user picked
  (setq v1 (list (- (car (car secPts)) (car basePt))
                 (- (cadr (car secPts)) (cadr basePt))))
  (setq v2 (list (- (car sidePt) (car basePt))
                 (- (cadr sidePt) (cadr basePt))))
  (setq cross (- (* (car v1) (cadr v2)) (* (cadr v1) (car v2))))
  (if (< cross 0)
    (setq stackDir (+ stackDir pi))
  )

  (setq i 1)
  (foreach pt secPts
    (setq offsetPt (polar basePt stackDir (* gap i)))
    (command "_.DIMLINEAR" basePt pt offsetPt "")
    (setq i (1+ i))
  )
  (princ)
)



;; Reload Lisps (RLL)
;; By Ryan Whitworth
;; Use this command to quickly reload this lisp file for testing.

;; Before using for the first time, you must:
;; Replace the file path below with your own, doubling every backslash (1 to 2, 2 to 4)
;; Make sure the file name itself is included in the path, not just the folder location. (Can right-click file & select "Copy as path")
;; Make sure quotation marks are on each end of the path.


(defun c:RLL ()
  (setq filePath "\\\\Forge-ahead\\hsi\\Project Management\\CAD Department\\Ryan W\\Lisps\\Ryan's Master Lisp.lsp") ; Change this to your file path
  (princ (strcat "\nFile path: " filePath))
  (if (findfile filePath)
    (progn
      (load filePath)
    ) ; end of progn
    (princ (strcat "\nError: File not found - " filePath))
  )
  (princ)
)



;; Place in your .lsp file below for testing

