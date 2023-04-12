module Clojure.UnitTests

open Clojure.Eval

printfn "Hello from F#"

let rt = ClojureRuntime()
let testCode = """

(ns maths.core
  ;(:require [quil.core :as q])
  )

(user/println "yo")
(def counter (atom 0))

(defn inc [n] (+ 1 n))
(swap! counter inc)

(deftype Point [^long x ^long y])
(defrecord Vec3 [])
(println "yo")

(def t (atom 0))
(defn math-abs [value]
  (try
    (if (< value 0) (* -1 value) value)
    (catch Exception e (println e))))

(defn math-max [a b] (if (> a b) a b))

;(deftype Vec3 [^double x ^double y ^double z])
;(deftype Quat [^double x ^double y ^double z ^double w])


(defrecord Vec3 [^double x ^double y ^double z])
(defrecord Quat [^double w ^double x ^double y ^double z])
(defn print-vec [^Vec3 v] (println v (type v) (:x v)))

(print-vec {:x 0 :y 0 :z 0})
"""

let lines = testCode.Split "\n"
try
  match FParsec.CharParsers.run Read.Parser.file testCode with
  | FParsec.CharParsers.ParserResult.Success(result, _, _) ->
    result |> List.iter ((fun id -> printfn "%s" lines.[int (snd id).Line - 1]; id) >> fst >> rt.Eval >> printfn "%A")
  | _else -> printfn "%A" _else
with error -> printfn "%A" error