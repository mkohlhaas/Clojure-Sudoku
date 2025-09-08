(ns sudoku.core
  (:require [clojure.set]))

(def rows
  [[0   1  2  3  4  5  6  7  8]
   [9  10 11 12 13 14 15 16 17]
   [18 19 20 21 22 23 24 25 26]
   [27 28 29 30 31 32 33 34 35]
   [36 37 38 39 40 41 42 43 44]
   [45 46 47 48 49 50 51 52 53]
   [54 55 56 57 58 59 60 61 62]
   [63 64 65 66 67 68 69 70 71]
   [72 73 74 75 76 77 78 79 80]])

(def cols
  [[0  9 18 27 36 45 54 63 72]
   [1 10 19 28 37 46 55 64 73]
   [2 11 20 29 38 47 56 65 74]
   [3 12 21 30 39 48 57 66 75]
   [4 13 22 31 40 49 58 67 76]
   [5 14 23 32 41 50 59 68 77]
   [6 15 24 33 42 51 60 69 78]
   [7 16 25 34 43 52 61 70 79]
   [8 17 26 35 44 53 62 71 80]])

(def quadrants
  [[0   1  2  9 10 11 18 19 20]
   [3   4  5 12 13 14 21 22 23]
   [6   7  8 15 16 17 24 25 26]
   [27 28 29 36 37 38 45 46 47]
   [30 31 32 39 40 41 48 49 50]
   [33 34 35 42 43 44 51 52 53]
   [54 55 56 63 64 65 72 73 74]
   [57 58 59 66 67 68 75 76 77]
   [60 61 62 69 70 71 78 79 80]])

(def blocks
  (concat rows cols quadrants))

(defn in?
  "true if vector contains item"
  [coll item]
  (some #(= item %) coll))

(defn idx-to-coord
  "coordinate of idx"
  [idx]
  ((juxt #(quot % 9) #(rem % 9)) idx))

(defn coord-to-idx
  "index of coordinate"
  [[row col]]
  (+ (* 9 row) col))

(defn board-solved?
  [board]
  (empty? (filter zero? (flatten board))))

(defn indexes-crossing-at
  "indexes of rows, cols, quadrants crossing at idx"
  [idx]
  (distinct
   (flatten
    (for [block blocks
          :when (in? block idx)]
      block))))

(defn possible-vals-at-hole
  "set of possible values at hole"
  [board hole]
  (clojure.set/difference
   (set (range 1 10))
   (set
    (for [idx (indexes-crossing-at (coord-to-idx hole))
          :let [val (get-in board (idx-to-coord idx))]
          :when (not (zero? val))]
      val))))

(defn all-holes
  "coordinates of all holes on board"
  [board]
  (for [x (range 9)
        y (range 9)
        :when (zero? (get-in board [x y]))]
    [x y]))

#_{:clojure-lsp/ignore [:clojure-lsp/unused-public-var]}
(defn first-hole
  "coordinate of first hole in board"
  [board]
  (first (all-holes board)))

;; most promising hole is the one with least number of possible values
(defn most-promising-hole
  "hole with the least number of possible values in row, col, quadrant"
  [board]
  (first
   (apply min-key second
          (for [hole (all-holes board)
                :let [num-possible-values (count (possible-vals-at-hole board hole))]]
            [hole num-possible-values]))))

(defn set-val-in-hole
  "new board with val in hole"
  [board hole val]
  (assoc-in board hole val))

(defn solve-sudoku-rec [[board & boards]]
  (if (board-solved? board)
    board
    (let [hole (most-promising-hole board)
          vals (possible-vals-at-hole board hole)]
      (recur (apply conj boards (map
                                 #(set-val-in-hole board hole %)
                                 vals))))))

(defn solve-sudoku [board]
  (solve-sudoku-rec [board]))
