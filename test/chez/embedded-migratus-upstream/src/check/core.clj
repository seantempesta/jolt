(ns check.core
  (:require [migratus.migrations :as migrations]))

(defn -main [& _]
  (let [found (migrations/find-migrations "migrations" #{"init.sql"} nil)
        expected {"001" {"create-users" {:sql {:down "DROP TABLE users;\n"
                                                    :up "CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT);\n"}}}
                  "002" {"add-email" {:sql {:up "ALTER TABLE users ADD COLUMN email TEXT;\n"}}}}]
    (when-not (= found expected)
      (throw (ex-info (str "unexpected Migratus migration map: " (pr-str found)) {:found found})))
    (prn :upstream-migratus-ok)))
