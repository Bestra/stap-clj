(ns stap-clj.core
  (:refer-clojure :exclude [* - + == /])
  (:use clojure.core.matrix)
  (:use clojure.core.matrix.operators)
  (:use clojure.core.matrix.linear)
  (:require [stap-clj.truss :as truss]))

(set-current-implementation :vectorz)

;; node-id x y z
(def node-data [[1 0 0 0]
                [2 100 100 0]
                [3 100 0 0]
                [4 200 0 0]])

;; element-id node1 node2 property-id
(def element-data [[1 1 2 1]
                   [2 1 3 1]
                   [3 2 3 1]
                   [4 2 4 1]
                   [5 3 4 1]])

;; node-id fx fy fz mx my mz
(def load-data
  [[2 1 2 0 0 0 0]])

;; node-id x y z mx my mz
(def boundary-data
  [[1 1 1 1 1 1 1]
   [4 1 1 1 1 1 1]])

(def material-data
  {1 {:A 1 :E 200000}})

(defn populate-boundaries
  "Copy the information from each boundary condition into the dof matrix"
  [empty-dofs boundary-data]
  (loop [boundaries boundary-data
         dofs empty-dofs]
    (if-let [[node-id boundary-data] (first boundaries)]
      (recur (rest boundaries) (set-row dofs (dec node-id) boundary-data))
      dofs)))

(defn enforce-plain-strain
  "Set rows 2-6 of the dof matrix to '1'"
  [dofs]
  (let [ones-column (matrix (repeat (row-count dofs) 1))]
    (loop [columns (range 2 6)
           dofs dofs]
      (if-let [column-idx (first columns)]
        (recur (rest columns) (set-column dofs column-idx ones-column))
        dofs))))

(defn setup-dof-array
 "Every degree of freedom in the system is numbered.  We start with
  an Nx6 array, where N is the number of nodes in the system.  Any given
  node could have a maximum of 6 available degrees of freedom.  A boundary
  condition on a DOF will remove it from the pool.  In a plane strain analysis
  only the first 2 DOFs for each node will be active"
 [node-data boundary-data plane-strain?]
 (let [empty-dofs (zero-matrix (row-count node-data) 6)
       boundary-dofs (populate-boundaries
                      empty-dofs
                      boundary-data)]
   (if plane-strain?
     (enforce-plain-strain boundary-dofs)
     boundary-dofs)))
