(ns stap-clj.truss
  (:require [stap-clj.util :as util]))

(defn el-coords
  "return xyz coordinates of a truss element's two nodes"
  [element-id]
  (let [element-row (dec element-id)
        n1 (.get @data/elements element-row 1)
        n2 (.get @data/elements element-row 2)]
    [(.viewRow @data/nodes (dec n1))
     (.viewRow @data/nodes (dec n2))]))

(defn make-st
  [vector length2]
  (util/append-negative-matrix (util/mmap vector #(/ % length2))))

(defn element-stiffness
  "calculate the stiffness matrix for an individual element"
  [el-id]
  (let [[n1 n2] (el-coords (el-id))
        truss-vector (util/v-from-to n1 n2)
        l-squared (util/vec-length2 truss-vector)
        l (Math/sqrt l-squared)
        e (get-in data/materials [1 :E])
        a (get-in data/materials [1 :A])
        xx (* e a l)
        st (make-st truss-vector l-squared)
        s (DenseDoubleMatrix1D. 21)
        c (atom 0)]
    (doseq [i (range 6)
          j (range i 6)
          :let [yy (* xx (.get st i))]]
      (do (.setQuick s @c (* yy (.get st j)))
          (swap! c inc)))
    s))

(defn setup-lm-matrix
  [elements dof-ids]
  (let [num-elements (.rows elements)
        lm (util/i-2 6 num-elements)]
    (doseq [el-index (range (.rows elements))]
      (let [n1 (.getQuick elements el-index 0)
            n2 (.getQuick elements el-index 1)
            n1-dofs (.viewColumn dof-ids (dec n1))
            n2-dofs (.viewColumn dof-ids (dec n2))
            combined-dofs (util/concat-i-1 n1-dofs n2-dofs)
            trace (do (prn el-index) el-index)
            lm-column (.viewColumn lm el-index)]
        (.assign lm-column combined-dofs)))
    lm))

(defn assemble-stiffness
  "assemble the stiffness matrix for truss elements"
  []
  ())

(defn calculate-stress
  "calculate the stress in the element given the nodal displacements"
  [el-id]
  (let [[n1 n2] (el-coords (el-id))
        truss-vector (util/v-from-to n1 n2)
        l-squared (util/vec-length2 truss-vector)
        e (get-in data/materials [1 :E])
        st (util/append-negative-matrix (util/mmap
                                         truss-vector
                                         #(* e
                                             (/ % l-squared))))
        ]))
