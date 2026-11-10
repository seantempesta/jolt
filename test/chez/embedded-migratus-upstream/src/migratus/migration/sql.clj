(ns migratus.migration.sql
  (:require [migratus.protocols :as proto]))
(defmethod proto/get-extension* :sql [_] "sql")
