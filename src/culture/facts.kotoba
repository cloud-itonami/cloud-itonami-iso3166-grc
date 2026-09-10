(ns culture.facts
  "Country-level regional-culture catalog for Greece (GRC) -- national
  dishes, protected products, beverages, crafts, festivals and heritage
  sites, per ADR-2607171400 addendum 2 (cloud-itonami-municipality-
  culture-catalog Wave 1, in com-junkawasaki/root). Sibling namespace to
  `marketentry.facts` / `statute.facts` (ADR-2607141700); city-level
  counterparts live in the cloud-itonami-municipality-* repos.

  Catalog is keyed by UPPERCASE ISO3 (mirrors `statute.facts`); entries
  carry no :culture/municipality (that attribute is city-level only).

  Every entry cites a source URL that was actually fetched and read on
  :culture/retrieved-at -- never fabricated. Summaries state only what the
  cited source confirms. An item not in this table has NO spec-basis, full
  stop; extend `catalog`, do not invent an id/url.")

(def catalog
  "iso3 -> vector of culture entries."
  {"GRC"
   [{:culture/id "grc.dish.moussaka"
     :culture/name "Moussaka"
     :culture/country "GRC"
     :culture/kind :dish
     :culture/summary "Layered aubergine- or potato-based dish with ground meat; the modern Greek version was created in the 1920s by chef Nikolaos Tselementes, and the dish has many regional variations across the Balkans and Middle East."
     :culture/url "https://en.wikipedia.org/wiki/Moussaka"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "grc.dish.souvlaki"
     :culture/name "Souvlaki"
     :culture/country "GRC"
     :culture/kind :dish
     :culture/summary "Greek food consisting of small pieces of meat and sometimes vegetables grilled on a skewer, typically eaten as fast food with pita bread."
     :culture/url "https://en.wikipedia.org/wiki/Souvlaki"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "grc.dish.greek-salad"
     :culture/name "Greek salad"
     :culture/country "GRC"
     :culture/kind :dish
     :culture/summary "Salad in Greek cuisine generally made with pieces of tomatoes, cucumbers, onion, feta cheese and olives, dressed with salt, oregano, lemon juice and olive oil."
     :culture/url "https://en.wikipedia.org/wiki/Greek_salad"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "grc.dish.spanakopita"
     :culture/name "Spanakopita"
     :culture/country "GRC"
     :culture/kind :dish
     :culture/summary "Savoury spinach pie of Greek origin with a filling of chopped spinach and usually feta or white cheese and egg, eaten throughout Greece."
     :culture/url "https://en.wikipedia.org/wiki/Savory_spinach_pie"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "grc.dish.gyros"
     :culture/name "Gyros"
     :culture/country "GRC"
     :culture/kind :dish
     :culture/summary "Meat cooked on a vertical rotisserie, sliced and served wrapped or stuffed in pita bread; the Greek variation developed after the 1922-23 population exchange with Turkey."
     :culture/url "https://en.wikipedia.org/wiki/Gyros"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "grc.beverage.ouzo"
     :culture/name "Ouzo"
     :culture/country "GRC"
     :culture/kind :beverage
     :culture/summary "Dry anise-flavoured aperitif spirit widely consumed in Greece and Cyprus, with EU protected geographical indication restricting production to Greece and Cyprus."
     :culture/url "https://en.wikipedia.org/wiki/Ouzo"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "grc.product.feta"
     :culture/name "Feta"
     :culture/country "GRC"
     :culture/kind :product
     :culture/summary "Greek brined white cheese made from sheep's milk or a mixture of sheep and goat's milk, with protected designation of origin (PDO) status since 2002."
     :culture/url "https://en.wikipedia.org/wiki/Feta"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "grc.festival.patras-carnival"
     :culture/name "Patras Carnival"
     :culture/country "GRC"
     :culture/kind :festival
     :culture/summary "The largest carnival event of its kind in Greece, first held in 1829, running from 17 January to Clean Monday with parades and the burning of a carnival-king effigy."
     :culture/url "https://en.wikipedia.org/wiki/Patras_Carnival"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "grc.heritage.acropolis-of-athens"
     :culture/name "Acropolis of Athens"
     :culture/country "GRC"
     :culture/kind :heritage
     :culture/summary "Ancient citadel above Athens containing the remains of the Parthenon and other historically significant buildings, designated a UNESCO World Heritage Site in 1987."
     :culture/url "https://en.wikipedia.org/wiki/Acropolis_of_Athens"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "grc.heritage.delphi"
     :culture/name "Delphi"
     :culture/country "GRC"
     :culture/kind :heritage
     :culture/summary "Ancient sacred precinct in central Greece, seat of the Pythia oracle, designated a UNESCO World Heritage Site in 1987."
     :culture/url "https://en.wikipedia.org/wiki/Delphi"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}]})

(defn spec-basis [iso3] (get catalog iso3))

(defn coverage
  ([] (coverage (keys catalog)))
  ([iso3s]
   (let [have (filter catalog iso3s)
         missing (remove catalog iso3s)]
     {:requested (count iso3s)
      :covered (count have)
      :covered-jurisdictions (vec (sort have))
      :missing-jurisdictions (vec (sort missing))
      :note (str "cloud-itonami-iso3166-grc culture catalog "
                 "(ADR-2607171400 addendum 2, Wave 1): " (count (get catalog "GRC"))
                 " GRC entries, each with a fetched-and-read citation. "
                 "Extend `culture.facts/catalog`, never fabricate an id/url.")})))

(defn by-kind [iso3 kind]
  (filterv #(= (:culture/kind %) kind) (spec-basis iso3)))
