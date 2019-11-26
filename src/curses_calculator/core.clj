(ns curses-calculator.core
  (:require [lanterna.screen :as s])
  (:gen-class))

(def screen-type :text)
(def main-screen (s/get-screen screen-type))

(def main-grid [["+" "-" "*" "/"]
                ["7" "8" "9" "="]
                ["4" "5" "6" "C"]
                ["1" "2" "3" "0"]])

(def main-grid-interface [[" + " " - " " * " " / "]
                          [" 7 " " 8 " " 9 " " = "]
                          [" 4 " " 5 " " 6 " " C "]
                          [" 1 " " 2 " " 3 " " 0 "]])

(def direction {\h [-1 0]
                \j [0 1]
                \k [0 -1]
                \l [1 0]})

(def grid-size [(dec (count (get main-grid 0)))
                (dec (count main-grid))])

(defn inside-grid?
  [position]
  (let [p (partition 2 (interleave position grid-size))]
    (every? (fn [[a b]] (<= 0 a b)) p)))

(defn change-position
  [current added]
  (let [new (map + current added)]
    (if (inside-grid? new)
      new
      current)))

(defn make-grid-interface
  [selected-position]
  (as-> main-grid-interface mgi
    (update-in mgi (reverse selected-position) clojure.string/replace #" " "^")
    (map #(str "|" (clojure.string/join "|" %) "|") mgi)))

(defn split-string
  [string]
  (let [s (if (= \- (first string)) (subs string 1) string)
        ns (rest (interleave (clojure.string/split s #"\d+")
                             (clojure.string/split s #"\D")))]
    (if (= \- (first string))
      (cons (str "-" (first ns)) (rest ns))
      ns)))

(defn calculate-vector
  [[v & vs]]
  (let [func (comp str eval read-string reduce)]
    [(func (fn [res [sign n]]
             (str "(" sign " " res " " n ")"))
           v
           (partition 2 vs))]))

(defn modify-result-vector
  [result-vector button positive?]
  (case button
    "=" (calculate-vector result-vector)
    "C" (if (not-empty result-vector) (pop result-vector) result-vector)
    ("+" "-" "*" "/") (if (and (not-empty result-vector)
                               (re-matches #"\D?\d+|\D?\d+\/\d+" (peek result-vector)))
                        (conj result-vector button)
                        result-vector)
    (if (and (not-empty result-vector)
             (re-matches #"\D?\d+|\D?\d+\/\d+" (peek result-vector)))
      (update result-vector (dec (count result-vector))
              str button)
      (conj result-vector (if positive? button (str "-" button))))))

(defn -main
  [& args]
  (println "Press <v> to view help")
  (read-line)
  (s/in-screen
   main-screen
   (loop [result-vector [] position [0 3] positive? true]
     (s/clear main-screen)
     (s/put-string main-screen 1 0 (str (if positive? "p " "n ")
                                        (clojure.string/join result-vector)))
     (s/put-sheet main-screen 0 1 (make-grid-interface position))
     (s/redraw main-screen)
     (let [key (s/get-key-blocking main-screen)]
       (case key
         (\h \j \k \l) (recur result-vector
                              (change-position position (direction key))
                              positive?)
         \n (recur result-vector position (not positive?))
         :enter (recur (modify-result-vector result-vector
                                             (get-in main-grid (reverse position))
                                             positive?)
                       position
                       true)
         \v (do
              (s/clear main-screen)
              (s/put-sheet main-screen 1 0 ["Move: hjkl" "Negative: n"
                                            "Help: v" "Quit: q"])
              (s/redraw main-screen)
              (s/get-key-blocking main-screen)
              (recur result-vector position positive?))
         \q (print "Bye :*")
         (recur result-vector position positive?))))))
