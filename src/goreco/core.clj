(ns goreco.core
  (:use plumbing.core
        fipe.core
        fipe.util)
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clojure.string :as str]
            [me.raynes.fs :as fs]
            [clojure.java.io :as io]
            [plumbing.graph :as graph]
            [clojure.math.numeric-tower :as math])
  (:gen-class))

(defn- some-span-matches
  "Does some span in the extraction arg match one of the multiple mentions in corefexpanded-arg?"
  [extraction-arg corefexpanded-arg]
  (some (fn [{:keys [sentidx sent-tok-span head-tok-span extent-tok-span]}]
          ((set (for [ts [sent-tok-span head-tok-span extent-tok-span]
                      :when ts]
                  [sentidx ts]))
             ((juxt :sentidx :sent-tok-span) extraction-arg))) 
        corefexpanded-arg))

(defn match-extractions [gold-corefexpanded sys-extractions]
  (for [[[docname corefexpanded-anns] [docname* extractions]] 
        (map vector
             (read-from-file gold-corefexpanded)
             (read-from-file sys-extractions))
        :let [_ (assert (= docname docname*))
              matched-extractions {:gold-rels-with-matches (vec corefexpanded-anns)
                                   :false-positives []}
              matched-extractions 
              (reduce
               (fn [{:keys [gold-rels-with-matches false-positives] :as matched-extractions}
                    {:keys [relation args] :as extraction}]
                 (let [matching-relidx 
                       (first
                        (for [[i rel] (indexed gold-rels-with-matches)
                              :when (and (= relation (:relation rel))
                                         (every? (partial apply some-span-matches) (map vector args (:args rel))))]
                          i))]
                   (if matching-relidx
                     (update-in matched-extractions 
                                [:gold-rels-with-matches matching-relidx :matched-extractions]
                                (fnil conj [])
                                extraction)
                     (update-in matched-extractions
                                [:false-positives]
                                conj
                                extraction))))
               matched-extractions
               extractions)]]
    [docname (update-in matched-extractions
                        [:gold-rels-with-matches]
                        (fn [rels] 
                          (for [r rels]
                            (dissoc r :args))))]))

;; TODO use extractions-vs-gold->flat
(defn matched-extractions->csv [extractions-vs-gold-file]
  (let [extractions-vs-gold (read-from-file extractions-vs-gold-file)]
    (cons ["Document" "Sentence" "Relation" "Arg1" "Arg2" "Gold
    Match" "Score"]
          (for [[docname {:keys [gold-rels-with-matches false-positives]}] extractions-vs-gold
                extraction (concat
                            (for [rel gold-rels-with-matches
                                  extraction (:matched-extractions rel)]
                              (assoc extraction :gold-match? true))
                            false-positives)]
            (concat 
             [docname 
              (:sentidx (first (:args extraction)))
              (:relation extraction)]
             (for [arg (:args extraction)]
               (str/join " | " ((juxt :text :ner-type :kbid) arg)))
             [(boolean (:gold-match? extraction))
              (:score extraction)
              (if-let [model-typesigs (:from-models extraction)]
                (str/join " " model-typesigs))])))))

(defn safe-div [x y]
  (try (/ x y)
       (catch ArithmeticException _
         (cond (> x 0)   Double/POSITIVE_INFINITY
               (zero? x) Double/NaN
               :else     Double/NEGATIVE_INFINITY))))

(def corefexpanded-eval-computations
  (graph/eager-compile
   {:extractions-count (fnk [filterfn extractions]
                            (->> extractions
                                 (map second)
                                 aconcat
                                 (filter filterfn)
                                 count))
    :gold-instances-count (fnk [filterfn extractions-vs-gold]
                               (->> extractions-vs-gold
                                    (map second)
                                    (map :gold-rels-with-matches)
                                    aconcat
                                    (filter filterfn)
                                    count))
    :false-positives-count (fnk [filterfn extractions-vs-gold]
                                (->> extractions-vs-gold
                                     (map second)
                                     (map :false-positives)
                                     aconcat
                                     (filter filterfn)
                                     count))
    :true-positives-count (fnk [filterfn extractions-vs-gold]
                               (->> extractions-vs-gold
                                    (map second)
                                    (map :gold-rels-with-matches)
                                    aconcat
                                    (filter :matched-extractions)
                                    (filter filterfn)
                                    (filter #(some filterfn (:matched-extractions %)))
                                    count))
    :precision (fnk [true-positives-count false-positives-count]
                    (double (safe-div true-positives-count 
                                      (+ true-positives-count
                                         false-positives-count)))) 
    :recall (fnk [true-positives-count gold-instances-count]
                 (double (safe-div true-positives-count gold-instances-count)))
    :f1 (fnk [precision recall]
             (double (safe-div (* 2 precision recall) (+ precision recall))))}))

(defn corefexpanded-perf [extractions extractions-vs-gold threshold]
  (assoc 
      (corefexpanded-eval-computations
       {:extractions extractions
        :extractions-vs-gold extractions-vs-gold
        :filterfn #(if (:score %) (>= (:score %) threshold) true)})
    :threshold threshold))

(defn summarize-extractions-vs-gold [extractions-file extractions-vs-gold-file]
  (corefexpanded-eval-computations 
   {:filterfn (constantly true)
    :extractions (read-from-file extractions-file)
    :extractions-vs-gold (read-from-file extractions-vs-gold-file)}))

(defn- get-curve-perfs [extractions-file extractions-vs-gold-file]
  (let [extractions (read-from-file extractions-file)
        extractions-vs-gold (read-from-file extractions-vs-gold-file)
        thresholds 
        (-> (for [[docname {:keys [gold-rels-with-matches false-positives]}] extractions-vs-gold
                  :let [extractions (concat
                                     (for [rel gold-rels-with-matches
                                           extraction (:matched-extractions rel)]
                                       extraction)
                                     false-positives)
                        scores (map :score extractions)]]
              scores)
            aconcat
            sort)]
    (pmap (partial corefexpanded-perf extractions extractions-vs-gold) thresholds)))

(defn make-prcurve [extractions-file extractions-vs-gold-file]
  (let [perfs (get-curve-perfs extractions-file extractions-vs-gold-file)]
    (concat
     [[1 0 Double/NEGATIVE_INFINITY]]
     (for [{:keys [precision recall threshold]} perfs]
       [recall precision threshold])
     [[0 1 Double/POSITIVE_INFINITY]])))

(defn make-p-tp-curve [extractions-file extractions-vs-gold-file]
  (let [perfs (get-curve-perfs extractions-file extractions-vs-gold-file)]
    (for [{:keys [precision true-positives-count threshold]} perfs]
      [true-positives-count precision threshold])))

(def tp-points (map #(math/round (* % 77)) (range 0.1 1 0.1)))

(defn make-p-tp-curve-coarse [extractions-file extractions-vs-gold-file]
  (let [perfs (get-curve-perfs extractions-file extractions-vs-gold-file)
        max-tp (apply max (map :true-positives-count perfs))]
    (for [[true-positives-count perf-opts] (sort-by first (vec (group-by :true-positives-count perfs)))
          :when ((set (conj tp-points max-tp)) true-positives-count)
          :let [{:keys [precision _ threshold]} (apply max-key :precision (mapv #(into {} %) perf-opts))]]
      [true-positives-count precision threshold])))

(defn get-best-thresh [extractions-file extractions-vs-gold-file]
  (let [perfs (get-curve-perfs extractions-file extractions-vs-gold-file)]
    (apply max-key :f1 (filter #(not (Double/isNaN (:f1 %))) perfs))))

(defn get-performance [extractions-file extractions-vs-gold-file bestthresh-file]
  (let [threshold (:threshold (first (read-from-file bestthresh-file)))]
    (corefexpanded-perf (read-from-file extractions-file)
                             (read-from-file extractions-vs-gold-file)
                             threshold)))

(defn compute-auc [prcurve-file]
  (let [prcurve (->> (read-from-file prcurve-file)
                     (map #(map (fn [x] (Double/parseDouble x)) %))
                     reverse    ; order for increasing recall (x-axis)
                     (map (partial take 2)) ; don't need thresholds
                     )]
    (sum
     (fn [[[x1 y1] [x2 y2]]]
       (* 0.5 (- x2 x1) (+ y2 y1))) ; trapezoid area
     (map vector prcurve (rest prcurve)))))

(defn by-rel-results-table [extractions-file extractions-vs-gold-file & [bestthresh-file]]
  (let [extractions (read-from-file extractions-file)
        extractions-vs-gold (read-from-file extractions-vs-gold-file)
        threshold (when bestthresh-file (:threshold (first (read-from-file bestthresh-file))))
        rels (->> extractions-vs-gold
                  (map second) (map :gold-rels-with-matches)
                  aconcat
                  (filter :matched-extractions) (map :matched-extractions)
                  aconcat
                  (map :relation) sort distinct vec)]
    (->> (for [rel rels]
           (assoc (corefexpanded-eval-computations 
                   {:filterfn (if threshold
                                #(and 
                                  (= rel (:relation %))
                                  (if (:score %) (>= (:score %) threshold) true))
                                #(= rel (:relation %)))
                    :extractions extractions
                    :extractions-vs-gold extractions-vs-gold})
             :rel rel))
         (sort-by :extractions-count)
         reverse)))

(defn by-rel-results-table->csv [in-file]
  (cons ["Relation" "#Extractions" "#FP" "#Matches" "P" "R" "F1"]
        (for [{:keys [rel extractions-count false-positives-count true-positives-count precision recall f1]} (read-from-file in-file)]
          [rel extractions-count false-positives-count true-positives-count precision recall f1])))

(def rel-short-name-map
  {"/people/person/profession" "Profession"
   "/people/person/employment_history|/business/employment_tenure/company" "EmployedBy"
   "/people/person/nationality" "Nationality"
   "/people/person/spouse_s|/people/marriage/spouse" "Spouse"
   "/organization/organization/headquarters|/location/mailing_address/citytown" "OrgInCitytown"
   "/people/person/places_lived|/people/place_lived/location" "LivedIn"
   "/people/person/place_of_birth" "BornIn"
   "/organization/organization/organization_type" "OrgType"
   "athletePlaysForTeam" "AthletePlaysForTeam"
   "teamPlaysInLeague" "TeamPlaysInLeague"
   "newspaperInCity" "NewspaperInCity"})

(defn rel-short-name [rel]
  (if-let [short-name (rel-short-name-map rel)]
    short-name
    rel))

(defn tex-row [row]
  (-> row
      (->> (str/join " & "))
      (str/replace "_" "\\_")
      (str/replace "#" "\\#")
      (str "\\\\")))

(defn by-rel-results-table->tex [in-file]
  (concat
   ["\\toprule"]
   [(tex-row ["Relation" "#Extractions" "#TP" "#FP" 
              #_("R" "P" "F1")])]
   ["\\midrule"]
   (for [{:keys [rel extractions-count false-positives-count true-positives-count precision recall f1]} (read-from-file in-file)]
     (tex-row
      (concat
       [(str "{\\sf " (rel-short-name rel) "}") extractions-count true-positives-count false-positives-count]
       #_(map (partial format "%.3f") [recall precision f1]))))
   ["\\bottomrule"]))

(defn efname [base-dir set-name suffix]
  (str (io/file base-dir (str set-name "-" suffix))))

(defn def-evaluation-fipe-targets
  "Define fipe targets for files relevant to evaluation given a fipe-relative base directory and the gold annotations directory and extractions file (not fipe-relative)."
  [base-dir set-name gold-anns-dir extractions-file]

  (let [efname (fn [suffix] (str (io/file base-dir (str set-name "-" suffix))))]
   
    ;; Match extractions against coref expanded gold
    (deftarget (efname "extractions-vs-gold.edn")
      (match-extractions
       (io/file gold-anns-dir (str set-name "-corefexpanded-anns.edn"))
       extractions-file))
    (deftarget (efname "extractions-vs-gold.csv")
      (matched-extractions->csv (dep (efname "extractions-vs-gold.edn"))))

    ;; Summarize results
    (deftarget (efname "extractions-vs-gold.summary.edn")
      (summarize-extractions-vs-gold
       extractions-file
       (dep (efname "extractions-vs-gold.edn"))))

    ;; P/R curve
    (deftarget (efname "prcurve.tsv")
      (make-prcurve 
       extractions-file
       (dep (efname "extractions-vs-gold.edn"))))
    (deftarget (efname "p-tp-curve.tsv")
      (make-p-tp-curve 
       extractions-file
       (dep (efname "extractions-vs-gold.edn"))))
    (deftarget (efname "p-tp-curve-coarse.tsv")
      (make-p-tp-curve-coarse
       extractions-file
       (dep (efname "extractions-vs-gold.edn"))))

    ;; AUC
    (deftarget (efname "auc.txt")
      (compute-auc (dep (efname "prcurve.tsv"))))
    
    ;; By-relation results table
    (deftarget (efname "by-rel-results-table.edn")
      (by-rel-results-table 
       extractions-file
       (dep (efname "extractions-vs-gold.edn"))))
    (deftarget (efname "by-rel-results-table.csv")
      (by-rel-results-table->csv (dep (efname "by-rel-results-table.edn"))))

    ))

(defn run-evaluate [set-name extractions-file]
  (let [eval-dir (str extractions-file "-goreco-evaluation")]
    (println (str "Creating directory " eval-dir "/"))
    (fs/mkdirs eval-dir)
    (set-fipe-dir! eval-dir)
    (def-evaluation-fipe-targets nil set-name "gold-annotations" extractions-file)
    (fipe-glob "*")))

(def cli-options
  [["-h" "--help"]])

(defn usage [options-summary]
  (->> ["GoReCo relation extraction evaluation"
        ""
        "Usage: ./goreco.sh [options] action"
        ""
        "Options:"
        options-summary
        ""
        "Actions:"
        "  evaluate    Evaluate extractions"]
       (str/join \newline)))

(defn error-msg [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (str/join \newline errors)))

(defn exit [status msg]
  (println msg)
  (System/exit status))

(defn -main [& args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
    ;; Handle help and error conditions
    (cond
     (:help options) (exit 0 (usage summary))
     errors (exit 1 (error-msg errors)))
    ;; Execute program with options
    (case (first arguments)
      "evaluate" (do (apply run-evaluate (subvec arguments 1 3))
                     (System/exit 0))
      (exit 1 (usage summary)))))
