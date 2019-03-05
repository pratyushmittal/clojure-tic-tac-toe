(ns tic-tac-toe.core
  (:require [clojure.string :as s]
            [clojure.core.logic :as logic]
            [clojure.core.logic.pldb :as pldb]
            [clojure.set :as set]))


; Relational db for facts
(pldb/db-rel turn player pos)


; All winning combinations
(def wins [[1 2 3]
           [4 5 6]
           [7 8 9]
           [1 5 9]
           [3 5 7]
           [1 4 7]
           [2 5 8]
           [3 6 9]])


(defn played
  "Places played by any given player"
  [turns player]
  (pldb/with-db turns
                (logic/run* [q]
                            (turn player q))))


(defn available-options
  "Unoccupied places available"
  [turns]
  (vec (set/difference #{1 2 3 4 5 6 7 8 9}
                       (played turns :human)
                       (played turns :computer))))


(defn win-turn
  "Find winning places"
  [turns player]
  (let [options (available-options turns)]
    (pldb/with-db turns
                  (logic/run* [q]
                              (logic/fresh [a b]
                                           (logic/conde
                                             [(logic/membero [a b q] wins)]
                                             [(logic/membero [a q b] wins)]
                                             [(logic/membero [q a b] wins)])
                                           (turn player a)
                                           (turn player b)
                                           (logic/membero q options))))))


(defn opponent-of
  "Returns opponent of the given player"
  [player]
  (if (= player :human)
    :computer
    :human))


(defn best-turn
  "Finds the best position to play"
  [turns player]
  (let [opponent (opponent-of player)
        win-turns (win-turn turns player)
        def-turns (win-turn turns opponent)
        avail-turns (available-options turns)
        selected (or (first win-turns)
                     (first def-turns)
                     (rand-nth avail-turns))]
    (println "Win turns" win-turns "\n"
             "Def turns" def-turns "\n"
             "Avail turns" avail-turns "\n"
             "Chosen" selected)
    selected))


(defn in?
  "Returns true if the item is in given vector or list"
  [coll item]
  (some #(= item %) coll))


(defn parse-input
  "Parses human input to a number"
  [text]
  (try
      (Integer. text)
     (catch Exception e
       text)))


(defn human-turn
  "Asks user for input"
  [turns]
  (println "Please play your turn")
  (let [input (parse-input (read-line))
        options (available-options turns)]
    (if-not (in? options input)
      (do (println "Please enter a valid position")
          (println "Valid positions are" options)
          (human-turn turns))
      input)))


(defn winner
  "Finds winning player"
  [turns]
  (pldb/with-db turns
                (logic/run* [player]
                            (logic/fresh [a b c]
                                         (logic/membero [a b c] wins)
                                         (turn player a)
                                         (turn player b)
                                         (turn player c)))))


(defn game-over
  "Returns true if the game is over"
  [turns]
  (let [no-positions? (empty? (available-options turns))
        winner       (first (winner turns))]
    (cond
      (and (not winner) (not no-positions?)) false
      winner (do (println winner "won|") true)
      :else (do (println "Game is a tie") true))))


(defn render-board
  "Renders playing grid"
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
  "Asks next player for their turn"
  [turns player]
  (let [pos     (if (= player :computer)
                  (best-turn turns :computer)
                  (human-turn turns))
        turns   (pldb/db-fact turns turn player pos)]
    (render-board turns)
    (if (game-over turns)
      (println "Game Over")
      (play turns (opponent-of player)))))


; Initialize the game play
(play (pldb/db) :human)
