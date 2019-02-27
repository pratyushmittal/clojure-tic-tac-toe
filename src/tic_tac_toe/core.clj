(ns tic-tac-toe.core
  (:require [clojure.string :as s]
            [clojure.core.logic :as logic]
            [clojure.core.logic.pldb :as pldb]
            [clojure.set :as set]))


; Relational db for facts
(pldb/db-rel turn player pos)


(def wins [[1 2 3]
           [4 5 6]
           [7 8 9]
           [1 5 9]
           [3 5 7]
           [1 4 7]
           [2 5 8]
           [3 6 9]])


(defn played
  [turns player]
  (pldb/with-db turns
                (logic/run* [q]
                            (turn player q))))


(defn available-options
  [turns]
  (set/difference #{1 2 3 4 5 6 7 8 9}
                  (played turns :human)
                  (played turns :computer)))


(defn win-turn
  [turns player]
  (pldb/with-db turns
                (logic/run* [q]
                            (logic/fresh [a b]
                                         (logic/conde
                                           [(logic/membero [a b q] wins)]
                                           [(logic/membero [a q b] wins)]
                                           [(logic/membero [q a b] wins)])
                                         (turn player a)
                                         (turn player b)))))


(defn opponent-of
  [player]
  (if (= player :human)
    :computer
    :human))


(defn best-turn
  [turns player]
  (let [opponent (opponent-of player)]
    (or (first (win-turn turns player))
        (first (win-turn turns opponent))
        (first (available-options turns)))))


(defn in?
  [coll item]
  (some #(= item %) coll))


(defn human-turn
  "Asks user for input"
  [turns]
  (println "Please play your turn")
  (let [input (Integer. (read-line))
        options (available-options turns)]
    (if-not (in? options input)
      (do (println "Please enter a valid position")
          (println "Valid positions are" options)
          (human-turn turns))
      input)))


(defn game-over
  [turns]
  (if (empty? (available-options turns))
    true
    false))


(defn render-board
  [turns]
  (let [human-turns (played turns :human)
        computer-turns (played turns :computer)
        marks (map (fn [i] (cond
                             (in? human-turns i) "X"
                             (in? computer-turns i) "O"
                             :else "_"))
                   (range 1 10))
        line-marks (map (fn [parts] (s/join "  " parts))
                        (partition 3 marks))
        layout (s/join "\n" line-marks)]
    (println layout "\n\n")))


(defn play
  [turns player]
  (let [pos     (if (= player :computer)
                  (best-turn turns :computer)
                  (human-turn turns))
        turns   (pldb/db-fact turns turn player pos)]
    (render-board turns)
    (if (game-over turns)
      (println "Game Over")
      (play turns (opponent-of player)))))


(play (pldb/db) :human)
