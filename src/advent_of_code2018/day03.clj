(ns advent-of-code2018.day03
  (:require [clojure.java.io :as io]))

;; --- Day 3: No Matter How You Slice It ---

;; The Elves managed to locate the chimney-squeeze prototype fabric for Santa's suit (thanks to someone who helpfully wrote its box IDs on the wall of the warehouse in the middle of the night). Unfortunately, anomalies are still affecting them - nobody can even agree on how to cut the fabric.

;; The whole piece of fabric they're working on is a very large square - at least 1000 inches on each side.

;; Each Elf has made a claim about which area of fabric would be ideal for Santa's suit. All claims have an ID and consist of a single rectangle with edges parallel to the edges of the fabric. Each claim's rectangle is defined as follows:

;; The number of inches between the left edge of the fabric and the left edge of the rectangle.
;; The number of inches between the top edge of the fabric and the top edge of the rectangle.
;; The width of the rectangle in inches.
;; The height of the rectangle in inches.
;; A claim like #123 @ 3,2: 5x4 means that claim ID 123 specifies a rectangle 3 inches from the left edge, 2 inches from the top edge, 5 inches wide, and 4 inches tall. Visually, it claims the square inches of fabric represented by # (and ignores the square inches of fabric represented by .) in the diagram below:

;; ...........
;; ...........
;; ...#####...
;; ...#####...
;; ...#####...
;; ...#####...
;; ...........
;; ...........
;; ...........
;; The problem is that many of the claims overlap, causing two or more claims to cover part of the same areas. For example, consider the following claims:

;; #1 @ 1,3: 4x4
;; #2 @ 3,1: 4x4
;; #3 @ 5,5: 2x2
;; Visually, these claim the following areas:

;; ........
;; ...2222.
;; ...2222.
;; .11XX22.
;; .11XX22.
;; .111133.
;; .111133.
;; ........
;; The four square inches marked with X are claimed by both 1 and 2. (Claim 3, while adjacent to the others, does not overlap either of them.)

;; If the Elves all proceed with their own plans, none of them will have enough fabric. How many square inches of fabric are within two or more claims?

(defrecord ElfPlan [id left top width height])

(defn parse-line [line]
  (->> (re-find #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)" line)
       (rest)
       (map #(Long/parseLong %))
       (apply ->ElfPlan)))

(comment
  (def plan1 (parse-line "#1 @ 1,3: 4x4"))
  (def plan2 (parse-line "#2 @ 3,1: 4x4"))
  (def plan3 (parse-line "#3 @ 5,5: 2x2")))

(defn read-input [resource-name]
  (->> (io/resource resource-name)
       (io/reader)
       (line-seq)
       (map parse-line)))

(defn squares [plan]
  (for [w (range (:width plan))
        h (range (:height plan))]
    [(+ w (:left plan))
     (+ h (:top plan))]))

(comment
  (squares plan1)
  (squares plan3))

(defn part-one []
  (->> (read-input "day03-1.txt")
       (mapcat squares)
       (frequencies)
       (filter #(> (val %) 1))
       (count)))

;; --- Part Two ---

;; Amidst the chaos, you notice that exactly one claim doesn't overlap by even a single square inch of fabric with any other claim. If you can somehow draw attention to it, maybe the Elves will be able to make Santa's suit after all!

;; For example, in the claims above, only claim 3 is intact after all claims are made.

;; What is the ID of the only claim that doesn't overlap?

(defn right [plan] (dec (+ (:left plan) (:width plan))))
(defn bottom [plan] (dec (+ (:top plan) (:height plan))))

(defn intact [plan1 plan2]
  (or (< (right plan1) (:left plan2))
      (> (:left plan1) (right plan2))
      (< (bottom plan1) (:top plan2))
      (> (:top plan1) (bottom plan2))))

(comment
  (intact plan1 plan2)
  (intact plan1 plan3))

(defn unique [plans]
  (loop [ps plans]
    (when-not (empty? ps)
      (let [plan1 (first ps)
            itself-or-intact? (some-fn #{plan1} #(intact plan1 %))]
        (if (every? itself-or-intact? plans)
            plan1
            (recur (rest ps)))))))

(comment
  (unique [plan1 plan2 plan3]))

(defn part-two []
  (-> (read-input "day03-2.txt")
      (unique)
      :id))
