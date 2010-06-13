(use 'clojure.contrib.seq-utils)

;;  (time (def dreps (tokyo-read-reps Repliers "/opt/data/twitter/tokyo/dreps.hdb")))
;;  (time (def dreps (->> dreps (map (fn [[k v]] [k (into (sorted-map) v)])) (into {}))))
;;  (time (def dments (tokyo-read-reps Repliers "/opt/data/twitter/tokyo/dments.hdb")))
;;  (time (def dments (->> dments (map (fn [[k v]] [k (into (sorted-map) v)])) (into {}))))

(defn sum [xs] (apply + xs))
(defn err [ & args]
  (doto System/err (.print (apply str args)) .flush))
(defn errln [ & args]
  (doto System/err (.println (apply str args)) .flush))

(defn reps-sorted1? [dreps & [progress]] 
  (loop [[[user ureps :as userdays] :as users] (seq dreps) i 0] 
    ;; TODO compact nested ifs into cond?
    (if ureps
      (if (loop [prev 0 [[day _] :as days] (seq ureps)] 
        (when (and progress (= 0 (mod i progress))) (print ".") (.flush System/out)) 
        (if day (if (<= prev day) 
          (recur day (next days)) 
            (do (println userdays) false)) true)) 
            (recur (next users) (inc i)) false) true)))

(defn sorted-by? [pred s] 
  (if-let [ns (next s)] 
    (let [[a b] s] 
      (and (pred a b) (recur pred ns))) 
    true)) 
        
(defn reps-sorted2? [dreps] 
 (every? #(sorted-by? >= (map first (val %))) dreps))    

(defn get-in-or [data keys default]
  "should be in core as (get-in data keys :default default)"
  (or (get-in data keys) default))  

(defstruct s-graph    :dreps :dments :dcaps :ustats)
(defstruct user-stats :soc :day :ins :outs :tot :bal)

(defn day-ranges [dreps]
  "find the day range when each user exists in dreps
  PRE: dreps must be sorted in adjacency lists by day!"
  ;; (assert (reps-sorted1? dreps))
  (->> dreps (map (fn [[user days]] 
    (let [[start _] (first days) [finish _] (last days)] 
      [user start finish])))))
      
(defn merge-day-ranges [dr1 dr2]
  "merge two day-ranges results"
  (let [mr2 (->> dr2 (map (fn [[u s f]] [u [s f]])) (into {}))]
    (->> dr1 (map (fn [[user s1 f1]]
      (let [[s2 f2 :as have2] (mr2 user)
        [start finish] (if have2 [(min s1 s2) (max f1 f2)] [s1 f1])]
          [user start finish]))))))
      
(defn day-starts [dreps]
  "group users by their starting day"
  (->> dreps day-ranges (partition-by second)))      



(defn get-soccap [ustats user]
  (let [stats (ustats user)]
  (if stats (:soc stats) 0.)))
  
  
(defn soc-user-day-sum [sgraph day user]
  "NB acc is a list, conj prepends to it"
  (let [
    {:keys [dreps dments ustats]} sgraph
    stats (ustats user)
    dr (get-in dreps [user day])
    dm (get-in dments [user day])
    ]
    (if (not (or dr dm))
      [nil stats]
      ;; we had edges this cycle -- now let's dance and compute the change!
      (let [        
        {:keys [soc day ins outs tot bal]} stats
                
        ;; find all those who talked to us in the past to whom we replied now
        out-sum (if dr (->> dr (map (fn [[to num]]
          (let [to-bal (get bal to 0)] (if (>= to-bal 0) 0.
            (let [to-soc (get-soccap ustats to)] (if (zero? to-soc) 0.
            ;; TODO if previous tot is 0, we'll zero the multiplication
            ;; should we update tot first, or assume 1 for multiplier as below?
            ;; guess for repayment default will never be needed, can assert that
            (let [to-tot (get tot to 1.)]
              (* num to-bal to-tot to-soc)))))))) sum) 0.)
              
        in-sum-back (if dm (->> dm (map (fn [[from num]]
          (let [from-bal (get bal from 0)] (if (<= from-bal 0) 0.
            (let [from-soc (get-soccap ustats from)] (if (zero? from-soc) 0.
            (let [from-tot (get tot from 1.)]
              (* num from-bal from-tot from-soc)))))))) sum) 0.)

        in-sum-all (if dm (->> dm (map (fn [[from num]] 
            (let [from-soc (get-soccap ustats from)] (if (zero? from-soc) 0.
            (let [from-tot (get tot from 1.)]
                (* num from-tot from-soc)))))) sum) 0.)
        
        terms [out-sum in-sum-back in-sum-all]
      
      	dm-  (when dm (->> dm (map (fn [[k v]] [k (- v)])) (into {})))
      	
			  ins  (if dr (merge-with + ins  dr) ins)
			  outs (if dm (merge-with + outs dm) outs)
			  tot  (merge-with + tot dr dm)
			  bal  (merge-with + bal dr dm-)
			  
			  ;; TODO can assoc only what changed instead of recreating
			  stats (struct user-stats soc day ins outs tot bal)
			  ]

      [terms stats]))))

(defn safe-divide [numer denom] (if (zero? denom) 0 (/ numer denom)))      
      
(defn soc-day [sgraph [alpha beta gamma :as params] day]
  (assert (every? number? params))
  (let [
    ;; TODO have sgraph as 
    {:keys [ustats dcaps]} sgraph
    ;; TODO does it make sense to carry users separately from ustats,
    ;; perhaps with the first day they appear in our data?
    users  (map first ustats)
    
    terms-stats (map (partial soc-user-day-sum sgraph day) users)
    sum-terms   (map first terms-stats)
    
    _ (errln "got sum-terms, length " (count sum-terms))
    
    ;; NB the nested map had to be doall'ed or ot caused StackOverflowError!
    norms (reduce (fn [sums terms] (doall (map + sums terms))) (remove nil? sum-terms))
             
    ustats (->> terms-stats (map (fn [user [numers stats]]
      (let [
        soc (:soc stats)
        soc (if numers 
      	(let [
      		_ (assert (= 3 (count numers)))
      		[outs* ins-back* ins-all* :as normalized] (map safe-divide numers norms)]
        	(assert (and (= 3 (count normalized)) (every? number? normalized)))
        	(+ (* alpha soc) (* (- 1. alpha) (+ (* beta outs*) 
          		(* (- 1. beta) (+ (* gamma ins-back*) (* (- 1. gamma) ins-all*)))))))
         (* alpha soc))
        stats (assoc stats :soc soc)
        ]
        [user stats])) users) (into {}))
             
    _ (errln "got ustats")
    
    ;; day in fn is the same day as soc-day param day
    dcaps (->> ustats (reduce (fn [res [user {:keys [day soc]}]] 
      (assoc! res user (assoc (or (res user) (sorted-map)) day soc))) 
      (transient dcaps)) persistent!) 
    ]
    (assoc sgraph :ustats ustats :dcaps dcaps)))
    

(defn soc-run [dreps dments & [alpha beta gamma soc-init]]
  (let [
    soc-init (or soc-init 1.0)
    alpha    (or alpha    0.00001)
    beta     (or beta     0.5)
    gamma    (or gamma    0.5)
    
    params [alpha beta gamma]
    dcaps {}
    ;; TODO we have to initialize each users as he appears with 1
    ustats {}
    sgraph (struct s-graph dreps dments dcaps ustats)
    
    dranges (->> [dreps dments] (map day-ranges) 
      (apply merge-day-ranges) (sort-by second))

    dstarts (->> dranges (partition-by second) 
      (map #(vector ((comp second first) %) (map first %)))
      (into (sorted-map)))
      
    first-day (->> dstarts first first)  
    last-day  (->> dranges (map last) (apply max))  
    ]
    
    (errln "doing days from " first-day " to " last-day)
    
    (->> (range first-day (inc last-day))
      (reduce (fn [sgraph day]
      ;; inject the users first appearing in this cycle
      (let [ustats (:ustats sgraph)
        new-users (dstarts day)
        _ (errln "adding " (count new-users) " new users on day " day)
        new-ustats (->> new-users 
          (map #(vector % (struct user-stats soc-init day {} {} {} {}))) 
          (into {}))
        ustats (merge ustats new-ustats)
        sgraph (assoc sgraph :ustats ustats)
        ]
        (errln "day " day)
        (soc-day sgraph params day))) 
      sgraph))))

;; (def ustats (:ustats sgraph))
;; (time (def ucap (->> ustats (map (fn [[user {:keys [soc]}]] [user soc])) (sort-by second >))))
;; (def ucapday (map (fn [[user soc]] [user soc (->> user ustats :day)]) ucap))
;; (->> ucapday (filter #(< (last %) 10)) (take 20))