(ns game.core
	(:use clojure.pprint))

(defn empty-board
	[w h]
	(vec (repeat w (vec (repeat h nil)))))

(defn populate
	"Change to :on each of the cells specified as [y, x] coordinates."
	[board living-cells]
	(reduce (fn [board coordinate]
		(assoc-in board coordinate :oo))
	board
	living-cells))

(def initial
	(populate (empty-board 6 6) #{[2 0] [2 1] [2 2] [1 2] [0 1]}))

(defn neighbors
	[[x y]]
	(for [dx [-1 0 1] dy [-1 0 1] :when (not= 0 dx dy)]
		[(+ dx x) (+ dy y)]))

(defn count-neighbors
	[board loc]
	(count (filter #(get-in board %) (neighbors loc))))

(defn indexed-step
	[board]
	(let [w (count board)
		h (count (first board))]
		(loop [new-board board x 0 y 0]
			(cond
				(>= x w) new-board
				(>= y h) (recur new-board (inc x) 0)
				:else
				(let [new-liveness
					(case (count-neighbors board [x y])
						2 (get-in board [x y])
						3 :oo
						nil)]
					(recur (assoc-in new-board [x y] new-liveness) x (inc y)))))))

(defn -main
	[]
	(-> (iterate indexed-step initial) (nth 5) pprint)
	nil)