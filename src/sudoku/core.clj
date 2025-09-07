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

(def squares
  [[0   1  2  9 10 11 18 19 20]
   [3   4  5 12 13 14 21 22 23]
   [6   7  8 15 16 17 24 25 26]
   [27 28 29 36 37 38 45 46 47]
   [30 31 32 39 40 41 48 49 50]
   [33 34 35 42 43 44 51 52 53]
   [54 55 56 63 64 65 72 73 74]
   [57 58 59 66 67 68 75 76 77]
   [60 61 62 69 70 71 78 79 80]])

(defn flip [op]
  (fn [& args]
    (apply op (reverse args))))

(defn idx-to-coord [idx]
  ((juxt (partial (flip quot) 9) (partial (flip rem) 9)) idx))

(defn coord-to-idx [[x y]]
  (+ 9 (* 9 x)))

(idx-to-coord 39) ; [4 3]

(def blocks
  (concat rows cols squares))

(defn in?
  "true if coll contains elm"
  [coll elm]
  (some #(= elm %) coll))

(def sample-board
  [[0 4 9 0 0 8 6 0 5]
   [0 0 3 0 0 7 0 0 0]
   [0 0 0 0 0 0 0 3 0]
   [0 0 0 4 0 0 8 0 0]
   [0 6 0 8 1 5 0 2 0]
   [0 0 1 0 0 9 0 0 0]
   [0 1 0 0 0 0 0 0 0]
   [0 0 0 6 0 0 4 0 0]
   [8 0 4 5 0 0 3 9 0]])

(defn solved? [board]
  (empty? (filter #(zero? %) (flatten board))))

(defn find-first-hole [board]
  (first
   (for [x (range 9)
         y (range 9)
         :when (zero? (get-in board [x y]))]
     [x y])))

(find-first-hole sample-board) ; [2 2]

(defn find-rows-cols-block [idx]
  (for [block blocks
        :when (in? block idx)]
    block))

;; TODO: wrong!!!
(flatten (find-rows-cols-block (coord-to-idx [2 2]))) ; (27 28 29 30 31 32 33 34 35 0 9 18 27 36 45 54 63 72 27 28 29 36 37 38 45 46 47)

(defn possible-vals [board idxs]
  (clojure.set/difference
   (set (range 1 10))
   (set
    (for [idx idxs
          :let [val (get-in board (idx-to-coord idx))]
          :when (not (zero? val))]
      val))))

(possible-vals sample-board '(27 28 29 30 31 32 33 34 35 0 9 18 27 36 45 54 63 72 27 28 29 36 37 38 45 46 47))
; #{7 6 3 2 5}

(defn solve-sudoku [[board & boards]]
  #p board
  #p (count boards)
  #p (first boards)
  (if (solved? board)
    board
    (let [hole-coord (find-first-hole board)
          hole       (coord-to-idx hole-coord)
          blocks     (flatten (find-rows-cols-block hole))
          vals       (possible-vals board blocks)]
      (solve-sudoku (apply conj boards (map #(assoc-in board hole-coord %) #p vals))))))

(solve-sudoku [sample-board])

(solved? sample-board)

(def sample-board
  [[7 2 6 4 9 3 8 1 5]
   [3 1 5 7 2 8 9 4 6]
   [4 8 0 6 5 1 2 3 7] ; hole at [2 2] - val = 9
   [8 5 2 1 4 7 6 9 3]
   [6 7 3 9 8 5 1 2 4]
   [9 4 1 3 6 2 7 5 8]
   [1 9 4 8 3 6 5 7 2]
   [5 6 7 2 1 4 3 8 9]
   [2 3 8 5 7 9 4 6 1]])
