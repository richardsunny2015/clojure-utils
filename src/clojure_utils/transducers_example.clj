(ns clojure-utils.transducers-example)

(let [nums (range 1000000000)
      xf (comp (map inc)
               (filter even?))]
  (time
   #_(->> nums
          (map inc)
          (filter even?)
          (reduce +))
   (transduce xf + nums)))