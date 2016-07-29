(ns assignment4b.core
  (:require [reagent.core :as reagent :refer [atom]]
            [schema.core :as s]))

(enable-console-print!)

(println "Edits to this text should show up in your developer console.")

;; define your app data so that it doesn't get over-written on reload
(defonce mode (atom {:text "None"}))

;r-atom to store the co-ordinates, number of click and current mode.
;if Click=0 its the first click
;and if click=1 it is the second click.
(def screen-info (reagent/atom {:x1 0 :y1 0 :x2 0 :y2 0 :clicks 0 :mode "none"}))

;scheme implemented to validate ratom data.
(def screen-info-schema
 {:x1 s/Int
  :y1 s/Int
  :x2 s/Int
  :y2 s/Int
  :clicks s/Int
  :mode s/Str})

(s/validate screen-info-schema @screen-info)

;atom to store all the shapes-to-draw
(def shapes-to-draw (atom []))


;function to calculate radius using Pythogorous theorem.
(defn calculate-radius [x1 y1 x2 y2]
 (Math/sqrt (+ (* (- x1 x2) (- x1 x2)) (* (- y1 y2) (- y1 y2)))))

;to draw a circle.
(defn draw-circle [x1 y1 x2 y2]
 [:circle {:cx x1 :cy y1
           :r (calculate-radius x1 y1 x2 y2)
           :fill "none"}])

;to draw a rectangle
;converts negetive height or width to positive values before drawing the shapes
(defn draw-rect [x1 y1 x2 y2]
 (let [height (- y2 y1) width (- x2 x1)]
  (if (and (> width 0) (> height 0))
   [:rect {:x x1 :y y1
           :width width
           :height height
           :fill "none"}]
    (if (and (> width 0) (< height 0))
     [:rect {:x x1 :y y2
             :width width
             :height (* height -1)
             :fill "none"}]
      (if (and (< width 0) (> height 0))
        [:rect {:x x2 :y y1
                :width (* width -1)
                :height height
                :fill "none"}]
        [:rect {:x x2 :y y2
                :width (* width -1)
                :height (* height -1)
                :fill "none"}])))))

;to draw a line
(defn draw-line [x1 y1 x2 y2]
 [:line {:x1 x1 :y1 y1
         :x2 x2 :y2 y2}])

;is called when line option is selected, add the mode in the vector with all other fields as 0
(defn line []
 (swap! shapes-to-draw conj {:x1 0 :y1 0 :x2 0 :y2 0 :mode "Line"})
 (reset! mode {:text "Line"})
 (reset! screen-info {:x1 0 :y1 0 :x2 0 :y2 0 :clicks 0 :mode "Line"}))

;is called when circle option is selected, add the mode in the vector with all other fields as 0
(defn circle []
 (swap! shapes-to-draw conj {:x1 0 :y1 0 :x2 0 :y2 0 :mode "Circle"})
 (reset! mode {:text "Circle"})
 (reset! screen-info {:x1 0 :y1 0 :x2 0 :y2 0 :clicks 0 :mode "Circle"}))

;is called when rectangle option is selected, add the mode in the vector with all other fields as 0
(defn rectangle []
 (swap! shapes-to-draw conj {:x1 0 :y1 0 :x2 0 :y2 0 :mode "Rectangle"})
 (reset! mode {:text "Rectangle"})
 (reset! screen-info {:x1 0 :y1 0 :x2 0 :y2 0 :clicks 0 :mode "Rectangle"}))

;is called when undo is selected
;pop two entries (one with coordinate values and other with only mode and all other entries as 0)
(defn undo[]
 (if (> (count @shapes-to-draw) 1)
  (do
   (swap! shapes-to-draw pop)
   (swap! shapes-to-draw pop)
   (reset! mode {:text (:mode (last @shapes-to-draw))})
   (reset! screen-info {:x1 0 :y1 0 :x2 0 :y2 0 :clicks 0 :mode (:mode (last @shapes-to-draw))}))

  (if (= (count @shapes-to-draw) 1)
   (do
    (swap! shapes-to-draw pop)
    (reset! mode {:text "None"})
    (reset! screen-info {:x1 0 :y1 0 :x2 0 :y2 0 :clicks 0 :mode "None"})))))


;is called when clear is selected
;reset the vector to an empty vector
(defn clear []
 (reset! shapes-to-draw [])
 (reset! mode {:text "None"})
 (reset! screen-info (atom {:x1 0 :y1 0 :x2 0 :y2 0 :clicks 0 :mode "None"})))


;draw line, rectangle or circle depending on what is selected
;and for producing the drag effect
(defn draw []
 (list
  (for [x @shapes-to-draw]
   (cond
     (= (:mode x) "Line")
      (draw-line (:x1 x) (:y1 x) (:x2 x) (:y2 x))
     (= (:mode x) "Rectangle")
      (draw-rect (:x1 x) (:y1 x) (:x2 x) (:y2 x))
     (= (:mode x) "Circle")
      (draw-circle (:x1 x) (:y1 x) (:x2 x) (:y2 x))))

  ;gives the drag effect on screen while drawing
   (cond
    (= (:mode @screen-info) "Line")
      (draw-line (:x1 @screen-info) (:y1 @screen-info) (:x2 @screen-info) (:y2 @screen-info))
    (= (:mode @screen-info) "Rectangle")
      (draw-rect (:x1 @screen-info) (:y1 @screen-info) (:x2 @screen-info) (:y2 @screen-info))
    (= (:mode @screen-info) "Circle")
      (draw-circle (:x1 @screen-info) (:y1 @screen-info) (:x2 @screen-info) (:y2 @screen-info)))))



(defn paint []
 [:div
  [:svg
    {:width 1274
     :height 600
     :stroke "black"
     :style {:position
             :fixed
             :top 0
             :left 1
             :border "black solid 2px"}
     :on-click
       #(if (not= (:mode @screen-info) "none")
      (if (= (:clicks @screen-info) 1)
       (do
        (swap! shapes-to-draw conj {:x1 (:x1 @screen-info) :y1 (:y1 @screen-info) :x2 (.-clientX %) :y2 (.-clientY %) :mode (:mode @screen-info)})
        (reset! screen-info {:x1 (:x1 @screen-info) :y1 (:y1 @screen-info) :x2 (.-clientX %) :y2  (.-clientY %) :clicks 0 :mode (:mode @screen-info)}))
       (reset! screen-info {:x1 (.-clientX %) :y1 (.-clientY %) :x2 (.-clientX %) :y2 (.-clientY %) :clicks 1 :mode (:mode @screen-info)})
      )
     )

   :on-mouse-move
    #(if(= (:clicks @screen-info) 1)
      (reset! screen-info {:x1 (:x1 @screen-info) :y1 (:y1 @screen-info) :x2 (.-clientX %) :y2 (.-clientY %) :clicks 1 :mode (:mode @screen-info)}))}

   (draw)]

  [:button
    {:on-click circle
     :style {:position
             :fixed
             :top 605
             :left 1
             :width 100
             :height 30}} "CIRCLE"]
  [:button
    {:on-click rectangle
     :style {:position
             :fixed
             :top 605
             :left 102
             :width 100
             :height 30}} "RECTANGLE"]
  [:button
    {:on-click line
     :style {:position
             :fixed
             :top 605
             :left 203
             :width 100
             :height 30}} "LINE"]
  [:button
    {:on-click undo
     :style {:position
             :fixed
             :top 605
             :left 304
             :width 100
             :height 30}} "UNDO"]
  [:button
    {:on-click clear
     :style {:position
             :fixed
             :top 605
             :left 405
             :width 100
             :height 30}} "CLEAR SCREEN"]

  [:h2 {:style {:font-family "Shruti"}} "Mode: " (:text  @mode)]])

(reagent/render-component [paint]
                          (. js/document (getElementById "app")))


(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
