(ns exfn.app
  (:require
   [reagent.dom :as dom]
   [exfn.subs]
   [exfn.events]
   [clojure.string :as str]
   [re-frame.core :as rf]))

;; DEV NOTES
;; npx shadow-cljs watch app
;; then open web page in browser
;; then connect to repl.

;; -- Reagent Components ------------------------------------------------------------
(defn code-editor []
  (let [source @(rf/subscribe [:source])
        total-lines (count (str/split-lines source))]
    [:div
     [:div.editor
      [:textarea.text-editor-line-nos {:readonly true
                                       :value (->> source
                                                   (str/split-lines)
                                                   (count)
                                                   (range)
                                                   (str/join "\n"))}]
      [:textarea.text-editor {:on-change #(rf/dispatch-sync [:update-source (-> % .-target .-value)])
                              :value     @(rf/subscribe [:source])}]]
     [:button.btn.btn-primary {:on-click #(rf/dispatch [:parse])} "Parse"]]))

(let [source "abc\ndef\nghi\njkl\nmno"]
  (->> source
       (str/split-lines)
       (count)
       (range)
       (str/join "\n")))

(defn code []
  [:div.code-holder
   (let [code @(rf/subscribe [:code])
         breakpoints @(rf/subscribe [:breakpoints])
         code-with-lines (zipmap (range (count code)) code)
         eip @(rf/subscribe [:eip])]
     [:table.code
      (for [[line-no code-line] code-with-lines]
        [:tr.code-line {:key line-no
              :style {:background-color (if (= eip line-no) "goldenrod" "white")}}
         [:td.breakpoint
          [:i.fas.fa-circle {:style {:color (if (some? (breakpoints line-no)) "red" "lightgray")}
                             :on-click #(rf/dispatch [:toggle-breakpoint line-no])}]]
         [:td.eip
          [:i.fas.fa-angle-double-right
           {:style {:visibility (if (= eip line-no) :visible :hidden)}}]]
         [:td.line-number
          line-no]
         [:td
          [:span
           [:label.instruction (first code-line)]
           (for [i (rest code-line)]
             (if (keyword? i)
               [:label.register i]
               [:label.value i]))]]])])])

;; -- App ---------------------------------------------------------------------------
(defn app []
  [:div.content
   [:h1 "CLJS ASM"]
   [:div.row
    [:div.col.col-lg-4
     [code-editor]]
    [:div.col.col-lg-4
     [code]]
    [:div.col.col-lg-4]]])

;; -- Dev Helpers -------------------------------------------------------------------
(comment (rf/dispatch-sync [:initialize]))

;; -- After-Load --------------------------------------------------------------------
;; Do this after the page has loaded.
;; Initialize the initial db state.
(defn ^:dev/after-load start
  []
  (dom/render [app]
              (.getElementById js/document "app")))

(defn ^:export init []
  (start))

(defonce initialize (rf/dispatch-sync [:initialize]))       ; dispatch the event which will create the initial state. 