(ns clojure-utils.xml
  (:require [clojure.data.xml :as xml]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as cstr]
            [medley.core :as m]))

(defn- replace-with-properties
  "Takes map m and replaces values like
   '${property}' with the correct property"
  [properties m]
  (m/map-kv
   (fn [k v]
     (let [property-match (re-matches #"\$\{(.*)\}" v)]
       [k
        (if (not (empty? property-match))
          (get properties (keyword (second property-match)))
          v)]))
   m))

(defn- xml->map
  "Converts format of parsed xml into a readable map"
  ([parsed-xml]
   (xml->map {} parsed-xml))
  ([properties parsed-xml]
   (->> parsed-xml
        (:content)
        (reduce #(assoc %1
                        (:tag %2)
                        (-> %2 :content (first)))
                {})
        (replace-with-properties properties))))

(defn- maven-dependencies->edn-dependencies
  "Converts maven dependencies as a map into
   a map for deps.edn"
  [deps {:keys [groupId artifactId version classifier]}]
  (let [dep-name (cond-> (str groupId "/" artifactId)
                   (not (nil? classifier)) (str "$" classifier)
                   :always (symbol))]
    (assoc deps dep-name {:mvn/version version})))

(defn parse-maven->edn
  "Assumes that maven has been wrapped in a root tag.
   Parses maven file into a list of dependencies that
   can be used by deps.edn. If there is a :properties
   tag, replaces values in :dependencies with the appropriate
   values from :properties."
  ([f]
   (parse-maven->edn f nil))
  ([f target-file]
   (with-open [r (io/reader f)]
     (let [parsed-xml (:content (xml/parse r))
           properties (->> parsed-xml
                           (m/find-first #(= :properties (:tag %)))
                           (xml->map))
           dependency-management (->> parsed-xml
                                      (m/find-first #(= :dependencyManagement
                                                        (:tag %)))
                                      :content
                                      (m/find-first #(= :dependencies
                                                        (:tag %)))
                                      :content
                                      (map (partial xml->map properties))
                                      (reduce #(assoc %1
                                                      (:groupId %2)
                                                      (:version %2))
                                              {}))
           dependencies (->> parsed-xml
                             (m/find-first #(= :dependencies (:tag %)))
                             :content
                             (map (partial xml->map properties))
                             (map
                              (fn [{:keys [groupId version] :as dependency}]
                                (cond-> dependency
                                  (and (nil? version)
                                       (contains? dependency-management
                                                  groupId))
                                  (assoc :version
                                         (get dependency-management
                                              groupId)))))
                             (reduce maven-dependencies->edn-dependencies {}))]
       (spit (or target-file "target/deps.edn")
             (-> (with-out-str (pprint/pprint {:deps dependencies}))
                 (cstr/replace "#:mvn{:version" "{:mvn/version")))))))