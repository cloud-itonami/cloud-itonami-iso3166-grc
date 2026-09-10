(ns marketentry.facts "Greece market-entry catalog.")
(def catalog
  {"GRC" {:name "Greece"
          :owner-authority "EAADHSY / ESIDIS"
          :legal-basis "Law 4412/2016; EU directives"
          :national-spec "ESIDIS / Promitheus + AFM/GEMI"
          :provenance "https://www.promitheus.gov.gr/"
          :required-evidence ["AFM/GEMI record" "ESIDIS registration record" "GEMI extract" "Authorized-representative record"]
          :rep-owner-authority "contracting authorities / EAADHSY"
          :rep-legal-basis "EU establishment or Greek GEMI entity for many procedures"
          :rep-provenance "https://www.promitheus.gov.gr/"
          :corporate-number-owner-authority "AADE / GEMI"
          :corporate-number-legal-basis "AFM / GEMI number"
          :corporate-number-provenance "https://www.aade.gr/"}
   "USA" {:name "United States" :owner-authority "GSA/SAM.gov" :legal-basis "FAR" :national-spec "SAM.gov" :provenance "https://sam.gov/"
          :required-evidence ["EIN record" "SAM.gov registration record" "State business registration record" "SAM UEI verification record"]}
   "CYP" {:name "Cyprus" :owner-authority "Treasury e-procurement" :legal-basis "PPL" :national-spec "eProcurement" :provenance "https://www.eprocurement.gov.cy/"
          :required-evidence ["Reg. number" "eProcurement registration" "ROC extract" "Authorized-representative record"]}
   "ITA" {:name "Italy" :owner-authority "ANAC/MePA" :legal-basis "Codice appalti" :national-spec "MePA" :provenance "https://www.acquistinretepa.it/"
          :required-evidence ["P.IVA record" "MePA registration" "CCIAA extract" "Authorized-representative record"]}})

(defn spec-basis [iso3] (get catalog iso3))
(defn coverage
  ([] (coverage (keys catalog)))
  ([iso3s]
   (let [have (filter catalog iso3s) missing (remove catalog iso3s)]
     {:requested (count iso3s) :covered (count have)
      :covered-jurisdictions (vec (sort have))
      :missing-jurisdictions (vec (sort missing))
      :note "R0 catalog seed"})))
(defn required-evidence-satisfied? [iso3 submitted]
  (when-let [{:keys [required-evidence]} (spec-basis iso3)]
    (= (count required-evidence) (count (filter (set submitted) required-evidence)))))
(defn evidence-checklist [iso3] (:required-evidence (spec-basis iso3) []))
(defn rep-spec-basis [iso3]
  (when-let [sb (spec-basis iso3)]
    (when (:rep-owner-authority sb)
      (select-keys sb [:rep-owner-authority :rep-legal-basis :rep-provenance]))))
(defn corporate-number-spec-basis [iso3]
  (when-let [sb (spec-basis iso3)]
    (when (:corporate-number-owner-authority sb)
      (select-keys sb [:corporate-number-owner-authority :corporate-number-legal-basis :corporate-number-provenance]))))
