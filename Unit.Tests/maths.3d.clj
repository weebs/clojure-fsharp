(ns maths.core
  ;(:require [quil.core :as q])
  )
;(require '[quil.core :as q])
(deftype Point [^long x ^long y])
(require '[clj-async-profiler.core :as prof])
(require '[quil.core :as q])
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
(print-vec (->Vec3 0 0 0))
(print-vec {:x 0 :y 0 :z 0})
(println "\u001b[38;2;255;255;255m")
;(defn vec3-mult)
;(foo (Point. 420 20))
(def p (ref {:x 420 :y 840}))
; (defn -main [& args] (foo "lol"))

; point : x y z
; vector : x y z
; quat : theta x y z

;[0, x, y, z] * [w, x, y, z] =>
(defn square [x] (* x x))
(defn vec-length [^Vec3 v] (Math/sqrt (+ (square (:x v)) (square (:y v)) (square (:z v)))))
(defn vec-dot ^double [^Vec3 a ^Vec3 b]
  (+ (* (:x a) (:x b))
     (* (:y a) (:y b))
     (* (:z a) (:z b))))
;(clojure.repl/dir)
;(clojure.repl/source-fn)
(defn vec-mult [^double s ^Vec3 v]
  {:x (* s (:x v))
   :y (* s (:y v))
   :z (* s (:z v))})
(defn vec-normalized [^Vec3 v]
  (let [s (vec-length v)]
    {
     :x (* (/ 1.0 s) (:x v))
     :y (* (/ 1.0 s) (:y v))
     :z (* (/ 1.0 s) (:z v))
     }))

(defn vec-map2 [f ^Vec3 a ^Vec3 b]
  {:x (f (:x a) (:x b))
   :y (f (:y a) (:y b))
   :z (f (:z a) (:z b))
   })
(defmacro vec-map [f a]
  `{:x (~f (:x ~a))
   :y (~f (:y ~a))
   :z (~f (:z ~a))})
(defn vec-subtr [^Vec3 a ^Vec3 b]
  {:x (- (:x a) (:x b))
   :y (- (:y a) (:y b))
   :z (- (:z a) (:z b))})
(defn vec-add [^Vec3 a ^Vec3 b]
  {:x (+ (:x a) (:x b))
   :y (+ (:y a) (:y b))
   :z (+ (:z a) (:z b))})
(defn vec3-to-map [^Vec3 v] {:x (.x v) :y (.y v) :z (.z v)})
(def deriv-step 0.0001)
;(defn estimate-normal [^Vec3 p sdf]
(defn estimate-normal [ ^Vec3 p sdf]
  ;(let [p (vec3-to-map p)])
  (vec-normalized
    {:x (sdf (vec-subtr (assoc p :x (+ (:x p) deriv-step)) (assoc p :x (- (:x p) deriv-step))))
     :y (sdf (vec-subtr (assoc p :y (+ (:y p) deriv-step)) (assoc p :y (- (:y p) deriv-step))))
     :z (sdf (vec-subtr (assoc p :z (+ (:z p) deriv-step)) (assoc p :z (- (:z p) deriv-step))))
     }))

(defn b-phong [^Vec3 position ^Vec3 lightPosition ^Vec3 ambient ^Vec3 diffuse] ())

(def ambientCoeff 0.1)
(def diffuseCoeff 0.9)
(def specularCoeff 1.0)
(def specularExponent 64.0)
(def lightPos {:x 40.0 :y 7.0 :z -20.0})
(def up {:x 0 :y 1 :z 0})
(def front {:x 1 :y 0 :z 0})

(def fov 45.0)

(defn phong [^Vec3 camera ^Vec3 position ^Vec3 lightPosition ambient ^Vec3 diffuse sdf]
  (let [
        lightPosition (Vec3. 40 -20 -10)
        normal (estimate-normal position sdf)

        toEye (vec-normalized (vec-subtr camera position))
        toLight (vec-normalized (vec-subtr lightPosition position))
        lambertian (Math/max (vec-dot normal toLight) 0.0)
        distance (vec-length (vec-subtr lightPosition position))
        diffuseFactor 7

        halfwayDir (vec-normalized (vec-add toLight toEye))
        ambientLight (vec-mult ambientCoeff normal)

        diffuseLight (vec-map (fn [n] (/ n (Math/sqrt distance))) (vec-mult (* diffuseFactor lambertian) diffuse))
        ; TODO specular lighting
        ]
    (vec-add {:x 0 :y 0 :z 0} (vec-add ambientLight diffuseLight))))
(def height 40)
(def width 80)
(def origin {:x 0.0 :y 0.0 :z 2.0})
(def sphere {:x 0.0 :y 0.0 :z 2.0})
(def radius 8.0)
;(def screen)
;static member dot (q0: dquat, q1: dquat) = q0.x * q1.x + q0.y * q1.y + q0.z * q1.z + q0.w * q1.w
;static member conjugate (q: dquat) = dquat(-q.x, -q.y, -q.z, q.w)
;static member normalize (q: dquat) = if dquat.length q > 0.0 then q / dquat.length(q) else q
;static member inverse (q: dquat)   = dquat.normalize <| dquat.conjugate q
;
;static member (*) (q0: dquat, q1: dquat) =
;let x =
; q0.w * q1.x +
; q0.x * q1.w +
; q0.y * q1.z -
; q0.z * q1.y

;let y =
; q0.w * q1.y +
; q0.y * q1.w +
; q0.z * q1.x -
; q0.x * q1.z

;let z =
; q0.w * q1.z +
; q0.z * q1.w +
; q0.x * q1.y -
; q0.y * q1.x
;let w =
; q0.w * q1.w -
; q0.x * q1.x -
; q0.y * q1.y -
; q0.z * q1.z
;dquat.normalize <| dquat(x, y, z, w)
;static member length (q: dquat)  = sqrt (q.x * q.x + q.y * q.y + q.z * q.z + q.w * q.w)


;static member normalize (q: dquat) = if dquat.length q > 0.0 then q / dquat.length(q) else q
(defn quat-mult-scalar ^Quat [^Quat q ^double s]
  {:x (* s (:x q))
   :y (* s (:y q))
   :z (* s (:z q))
   :w (* s (:w q))})
(defn quat-len ^double [^Quat q]
  (Math/sqrt
    (+ (* (:x q) (:x q))
       (* (:y q) (:y q))
       (* (:z q) (:z q))
       (* (:w q) (:w q)))))
(defn quat-normalize ^Quat [^Quat q]
  (if (> (quat-len q) 0.0)
    (quat-mult-scalar q (/ 1.0 (quat-len q)))
    q
  ))
(defn quat-mult ^Quat [^Quat q0 ^Quat q1]
  (let [x (+ (* (:w q0) (:x q1))
             (* (:x q0) (:w q1))
             (* (:y q0) (:z q1))
             (* -1.0 (* (:z q0) (:y q1))))
        y (+ (* (:w q0) (:y q1))
             (* (:y q0) (:w q1))
             (* (:z q0) (:x q1))
             (* -1.0 (* (:x q0) (:z q1))))
        z (+ (* (:w q0) (:z q1))
             (* (:z q0) (:w q1))
             (* (:x q0) (:y q1))
             (* -1.0 (* (:y q0) (:x q1))))
        w (- (* 1.0 (* (:w q0) (:w q1)))
             (* (:x q0) (:x q1))
             (* (:y q0) (:y q1))
             (* (:z q0) (:z q1))
             )]
    ;(println "x = " (:w q0) (:x q1) (:x q0) (:w q1) (:y q0) (:z q1) (:z q0) (:y q1))
    ;(println "x = " (* (:w q0) (:x q1)) (* (:x q0) (:w q1)) (* (:y q0) (:z q1)) (* (:z q0) (:y q1)))
    ;(println "x = " x)
    ;(println "w = " (:w q0) (:w q1) (:x q0) (:x q1) (:y q0) (:y q1) (:z q0) (:z q1))
    ;(println "w = " (* (:w q0) (:w q1)) (* (:x q0) (:x q1)) (* (:y q0) (:y q1)) (* (:z q0) (:z q1)))
    ;(println "w = " w)
    ;(println "w = " (- (* (:w q0) (:w q1)) (* (:x q0) (:x q1))))
    ;(println q0 " * " q1)
    ;(println x y z w)
    ;(println "z mult =" z)
    (quat-normalize {:x x :y y :z z :w w})))

(defn quat-conj ^Quat [^Quat q]
  {:x (* -1.0 (:x q))
   :y (* -1.0 (:y q))
   :z (* -1.0 (:z q))
   :w (:w q)})
(defn rotate-point ^Quat [^Vec3 p ^Quat q]
  (let [P (quat-normalize (assoc p :w 0.0))
        factor (quat-len (assoc p :w 0.0))
        ;factor (/ 1.0 l)
        q1 (quat-conj q)
        r (quat-mult-scalar (quat-mult (quat-mult q P) q1) factor)]
    ;(println "Normal " P)
    ;(println "q = " q)
    ;(println "First " (quat-mult q P))
    ;(println "Conj" q1)
    ;(println "Second " (quat-mult (quat-mult q P) q1))
    (dissoc r :w)))
(defn cast-ray [sdfFromPoint ^Vec3 startingPoint ^Vec3 directionVector ^Vec3 lightPos]
  (loop [n 0
         point startingPoint
         minStep 500
         ]
    (let [distance (sdfFromPoint point)
          ]
      (cond
        (> n 10000) nil

        (< (Math/abs distance) 0.01)
        (let [normal (estimate-normal point sdfFromPoint)
              color (phong {:x 0 :y 0 :z 2} point lightPos normal {:x 0 :y 128 :z 220} sdfFromPoint)]
          color
          ;{:x 0 :y 0 :z 0}
          )

        (< distance 500)
        (recur (+ n 1)  (vec-add point (vec-mult distance directionVector)) (min distance minStep))

        :else
        (if (< minStep 0.2) (vec-mult (* (/ minStep 0.2)) {:x 255 :y 0 :z 255}) nil)
        ))))
(defn draw-sphere []
  (doseq [y (range height)]
    (do
      (doseq [x (range width)]
        (let [x (float x) y (float y)
              ;px (- (* width (/ x width)) (* 0.5 width))
              px (+ (- x (/ width 2.0)) 0.0)
              py (- (+ (* -1.0 y) (/ height 2.0)) 0.0)
              p {:x px :y py :z 0.0}
              dir (vec-normalized (vec-subtr p origin))
              ;info (cast-ray #(sdf-sphere % sphere radius) p dir lightPos)
              info nil
              ]

          (cond
            (= nil info) (printf " ")
            :else (printf "-")))
          ;(let [hit (loop [p p n 0]
          ;            (when (and (= px 0.0) (= py 0.0))
          ;              ;(println "zeros")
          ;              ;(println dir)
          ;              ;(println x y p)
          ;              )
          ;            (cond
          ;              (> n 10000) " "
          ;              (< (Math/abs (sdf-sphere p sphere radius)) 0.00001) "*"
          ;              (< (:z p) 100.0)
          ;              (recur
          ;                (vec-add (vec-mult (sdf-sphere p sphere radius) dir) p)
          ;                (+ n 1))
          ;              :else " "
          ;              ))]
          ;  (printf hit)
          ;  )
          ;(println (distance p {:x 0.0 :y 0.0 :z 2.0} 0.1))
          ;(println x y)
          ;(println px py)
          )
        )
      (println ""))
    )


;(println screen)
(< 0.1 1)
(let [p {:x 1.0 :y 2.0 :z 0.0}
      q {:x 1.0 :y 0.0 :z 0.0 :w 1.0}
      l (quat-len (assoc p :w 0.0))
      pN (quat-normalize (assoc p :w 0.0))
      ;first (quat-mult q (assoc p :w 0.0))
      ;p' (rotate-point p q)
      ]
  (println "")
  (println "Vector length = " l)
  (println "pN =" pN)
  ;(println "p' = " p')

  ;(println "first =" (quat-mult q (assoc p :w 0.0)))
  (println "first =" (quat-mult q pN))
  (println "conj = " (quat-conj q))

  (println "second =" (quat-mult (quat-mult q pN) (quat-conj q)))
  (println "rot = " (rotate-point p q))

  (println "")
  ;(println "Result1337 =" (rotate-point pN q))
  )

(defn sdf-sphere [^Vec3 p ^Vec3 c ^double r]
  (try
    (- (vec-length (vec-subtr c p)) r)
    (catch Exception e (println e))))
(defn sdf-box [origin widthHeight point]
  (let [rotation {:x (+ @t 1.2) :y 1 :z 0 :w 1}
        offsetFromShapeOrigin (rotate-point (vec-subtr point origin) rotation)
        ;offsetFromShapeOrigin (vec-subtr point origin)
        distance (vec-subtr offsetFromShapeOrigin widthHeight)

        glslDistance (vec-subtr (vec-map math-abs offsetFromShapeOrigin) widthHeight)

        x (:x distance) y (:y distance) z (:z distance)
        ]

    (vec-length (vec-map2 math-max glslDistance {:x 0 :y 0 :z 0}))
    ))
;(prof/profile (draw-cuboid #(sdf-box {:x 4 :y 2 :z 12} {:x 8 :y 8 :z 8} %)))
;(prof/profile (draw-cuboid #(sdf-sphere % {:x 4 :y 2 :z 14} 24)))
;(draw-shape #(sdf-box {:x 2 :y 0 :z 20} {:x 20 :y 8 :z 8} % {:x 1 :y 0 :z 0 :w 1}))

;(prof/profile (draw-sphere))

(def results (atom nil))
(defn pixel-to-coord ^Vec3 [^Number x ^Number y ^Number width ^Number height]
  (let [widthMeters 0.71
        heightMeters 0.43]
    (Vec3.
      (- (* widthMeters (/ x width)) (/ widthMeters 2))
      (+ (/ heightMeters 2) (* heightMeters (/ (* -1 y) height)))
      0.0
     )))
(defn sdf-map [screenWidth screenHeight draw]
  (let [                                                    ;coords (pixel-to-coord x y screenWidth screenHeight)
        items (for [i (range (* screenWidth screenHeight))] {:x (mod i screenWidth) :y (int (/ i screenWidth))})]
    (pmap #(assoc % :result (draw (:x %) (:y %))) items)))

(defn make-draw [screenWidth screenHeight sdf]
  (fn [x y]
    (let [p (assoc (pixel-to-coord x y screenWidth screenHeight) :z 0)
          dir (vec-normalized (vec-subtr p (Vec3. 0 0 2)))]
      (cast-ray sdf p dir lightPos))))
(defn quil-shape [t screenWidth screenHeight sdf]
  (let [draw (make-draw screenWidth screenHeight sdf)
        results (sdf-map screenWidth screenHeight draw)
        ;items (for [i (range (* screenWidth screenHeight))] {:x (mod i screenWidth) :y (int (/ i screenWidth))})
        ;results
        ;(if (= nil @results)
        ;  (do
        ;    (reset! results (pmap #(assoc % :result (draw (:x %) (:y %))) items))
        ;    @results)
        ;  @results)
        ]
    (doseq [result results]
      (let [info (:result result) x (:x result) y (:y result)]
        ;(quil.core/clear)
        (cond
          (= nil info)
          (do
            ;(quil.core/stroke 0)
            ;(quil.core/fill 0)
            ;(quil.core/rect x y 1 1)
            ()
            )
          :else
          (do
            (quil.core/stroke (:x info) (:y info) (:z info))
            ;(quil.core/fill (:x info) (:y info) (:z info))
            (quil.core/rect x y 1 1)
            )
          )
        )))
  )
(defn console-draw-shape [screenWidth screenHeight sdf]
  (let [
        ;screenHeight (* 2 screenHeight)
        console-draw (fn [x y]
               (let [p (pixel-to-coord x y screenWidth screenHeight)
                     dir (vec-normalized (vec-subtr p {:x 0.0 :y 0.0 :z 2.0}))
                     info (cast-ray sdf p dir lightPos)]
                 info))
        ;draw (make-draw screenWidth screenHeight sdf)
        results (sdf-map screenWidth screenHeight console-draw)
        ;sdf #(sdf-box {:x 4 :y 2 :z 12} {:x 8 :y 8 :z 8} %)
        ]
    (doseq [result results]
      (let [info2 (:result result)]
        (when (= 0 (:x result))
          (println ""))
        (cond
          (= nil (:result result))
          (if (even? (:y result))
            (printf "\u001b[48;2;0;0;0m")
            (printf "\u001b[38;2;0;0;0m\u2580"))
          :else
          (if (even? (:y result))
            ;(printf "\u001b[48;2;0;0;0m")
            (printf (str "\u001b[48;2;" (int (:x info2)) ";" (int (:y info2)) ";" (int (:z info2)) "m"))
            (printf (str "\u001b[38;2;" (int (:x info2)) ";" (int (:y info2)) ";" (int (:z info2)) "m" "\u2580"))))
        ;(when (= 0 (clojure.core/mod (:x result) screenWidth)
        ;         (println "")))
        )))
  (println "done"))
  ;)
    ;(do
    ;  (cond
    ;    ;(= nil info2) (printf "\u001b[48;2;0;0;0m")
    ;    (= nil info2) (printf "\u001b[48;2;0;0;0m")
    ;    ;:else (printf "\u001b[48;2;0;0;200m")
    ;    :else
    ;    (printf (str "\u001b[48;2;" (int (:x info2)) ";" (int (:y info2)) ";" (int (:z info2)) "m"))
    ;    )
    ;  (cond
    ;    (= nil info) (printf "\u001b[38;2;0;0;0m ")
    ;    :else
    ;    (printf (str "\u001b[38;2;" (int (:x info)) ";" (int (:y info)) ";" (int (:z info)) "m" "\u2580"))))
    ;(doseq [y (range height)]
    ;  (doseq [x (range width)]
    ;    (let [x (float x) y (float (* 2 y))
    ;          ;px (+ (- x (/ width 2.0)) 0.0)
    ;          ;x (* (/ 0.71 (/ x width)))
    ;          ;y (* (/ 0.43 (/ y height)))
    ;          ;width 0.71
    ;          ;height 0.43
    ;
    ;          ; TODO
    ;          ;px (* 0.7 (- x (/ width 2.0)))
    ;          ;py (* 0.5 (+ (* -1.0 y) (/ height 2.0)))
    ;          ;px2 (* 0.7 (- (- x 0.5) (/ width 2.0)))
    ;          ;py2 (* 0.5 (+ (* -1.0 (+ y 0.5)) (/ height 2.0)))
    ;          ;
    ;          ;py (- (+ (* -1.0 y) (/ height 2.0)) 0.0)
    ;
    ;          ;p {:x px :y py :z 0.0}
    ;          ;dir (vec-normalized (vec-subtr p {:x 0.0 :y 0.0 :z 2.0}))
    ;          ;info (cast-ray sdf p dir lightPos)
    ;          ;interp {:x px2 :y py2 :z 0.0}
    ;          ;dir2 (vec-normalized (vec-subtr interp {:x 0.0 :y 0.0 :z 2.0}))
    ;          ;info2 (cast-ray sdf interp dir2 lightPos)
    ;
    ;          p (pixel-to-coord x y screenWidth screenHeight)
    ;          dir (vec-normalized (vec-subtr p {:x 0.0 :y 0.0 :z 2.0}))
    ;          info (cast-ray sdf p dir lightPos)
    ;
    ;          interp (pixel-to-coord x (+ 1 y) screenWidth screenHeight)
    ;          dir2 (vec-normalized (vec-subtr interp {:x 0.0 :y 0.0 :z 2.0}))
    ;          info2 (cast-ray sdf interp dir2 lightPos)
    ;          ]
    ;      ))
    ;  (println "")
    ;  ;(println "\u001b[38;2;0;255;255m" "\u001b[48;2;0;0;0m")
    ;  )
(console-draw-shape 80 40 #(sdf-sphere % {:x 0 :y -24 :z -80} 800))


(defn setup []
  (q/frame-rate 4)                    ;; Set framerate to 1 FPS
  (q/background 255))                 ;; Set the background colour to
;; a nice shade of grey.

(def frame (atom 0))
(defn draw []
  ;(quil-shape (* 10 @frame) 320 200 #(sdf-sphere % {:x 0 :y 0 :z 40} 0.4))
  (quil-shape (* 10 @frame) 320 200 #(sdf-box {:x 0 :y 0 :z -20} {:x 1 :y 1 :z 1} %))
  (swap! t inc)
  (swap! frame inc)
  ;(quil-shape 320 200 #(sdf-box {:x 0 :y 2 :z 2} {:x 8 :y 8 :z 8} %))
  ;(q/stroke (q/random 255))             ;; Set the stroke colour to a random grey
  ;(q/stroke-weight (q/random 10))       ;; Set the stroke thickness randomly

  (q/fill (q/random 255))                                 ;; Set the fill colour to a random grey

  ;(let [diam (q/random 100)             ;; Set the diameter to a value between 0 and 100
  ;      x    (q/random (q/width))       ;; Set the x coord randomly within the sketch
  ;      y    (q/random (q/height))]     ;; Set the y coord randomly within the sketch
  ;  (q/ellipse x y diam diam))
  )
;(if (or (not (resolve 'sketch)) (= nil (eval 'sketch)))
  (def sketch (q/defsketch example                          ;; Define a new sketch named example
                           :title "SDF in Clojure"          ;; Set the title of the sketch
                           :settings #(q/smooth 2)          ;; Turn on anti-aliasing
                           :setup setup                     ;; Specify the setup fn
                           :draw draw                       ;; Specify the draw fn
                           :size [323 200])
    )                ;; You struggle to beat the golden ratio
  (reset! results nil)
;; Draw a circle at x y with the correct diameter
