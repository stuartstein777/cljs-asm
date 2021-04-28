(ns exfn.app
  (:require
   [reagent.dom :as dom]
   [exfn.subs]
   [exfn.events]
   [exfn.helpers :as h]
   [re-frame.core :as rf]))

;; DEV NOTES
;; npx shadow-cljs watch app
;; then open web page in browser
;; then connect to repl.

;; -- Reagent Components ------------------------------------------------------------

;; Source Code Editor.
(defn code-editor []
  (let [source @(rf/subscribe [:source])]
    [:div
     [:div.editor
      [:div.source-editor-header "Source Editor"]
      [:textarea#lineNos.text-editor-line-nos {:readOnly  true
                                               :value     (h/get-source-line-numbers source)}]
      [:textarea#editor.text-editor {:on-change #(rf/dispatch-sync [:update-source (-> % .-target .-value)])
                                     :on-scroll (fn [^js e]
                                                  (let [scroll-pos (.. e -target -scrollTop)]
                                                    (rf/dispatch [:update-scroll scroll-pos])))
                                     :value     @(rf/subscribe [:source])
                                     :wrap      :off}]]]))

;; Display the parsed code.
(defn code []
  [:div#code-container.code-container
   [:div.parsed-code-header "Parsed Code"]
   (let [code @(rf/subscribe [:code])
         breakpoints @(rf/subscribe [:breakpoints])
         code-with-lines (zipmap (range (count code)) code)
         eip @(rf/subscribe [:eip])]
     [:table#code.code
      [:tbody
       (for [[line-no code-line] code-with-lines]
         [:tr.code-line {:key line-no
                         :style {:background-color (if (= eip line-no) "goldenrod" "white")}}
          [:td.breakpoint
           [:i.fas.fa-circle {:style {:color (if (some? (breakpoints line-no)) "red" "lightgray")}
                              :on-click #(rf/dispatch [:toggle-breakpoint line-no])}]]
          [:td.code-eip
           [:i.fas.fa-angle-double-right
            {:style {:visibility (if (= eip line-no) :visible :hidden)}}]]
          [:td.line-number [:div {:style {:height 25}}
                            line-no]]
          [:td
           [:span
            [:label.instruction (first code-line)]
            (let [arguments (rest code-line)]
              (for [i (zipmap (range (count arguments)) arguments)]
                (if (keyword? (val i))
                  [:label.register {:key (key i)} (val i)]
                  [:label.value {:key (key i)} (val i)])))]]])]])])

;; Buttons that control the executing code (start/stop/pause/speed)
(defn execution-controls []
  [:div.row
   (let [is-running? @(rf/subscribe [:running?])
         finished? @(rf/subscribe [:finished?])
         has-parsed-code? @(rf/subscribe [:has-parsed-code?])]
     [:div.execution-controls
      [:button.btn.btn-success.play-pause
       {:on-click #(rf/dispatch [:toggle-running])
        :disabled (or finished? (not has-parsed-code?))}
       (if is-running? [:i.fas.fa-pause] [:i.fas.fa-play])]
      [:button.btn.btn-success.next-instruction
       {:on-click #(rf/dispatch [:next-instruction])
        :disabled (or finished? (not has-parsed-code?))}
       [:i.fas.fa-forward]]
      [:button.btn.btn-danger.stop-button
       {:on-click #(rf/dispatch [:reset])}
       [:i.fas.fa-stop]]
      [:input.instr-per-sec {:type "text"
                             :placeholder "1"}]
      [:label.speed-label "speed (secs / instruction.)"]])])

;; Display the current eip when running.
(defn eip []
  (let [eip @(rf/subscribe [:eip])]
    [:div
     [:label.eip-header "EIP"]
     [:label.eip eip]]))

;; Display the user registers.
(defn registers []
  (let [registers @(rf/subscribe [:registers])]
    [:div.registers-container
     [:div.registers-header "Registers"]
     [:div.registers-list
      (when (not= registers {})
        (for [[k [name v]] (zipmap (range (count registers)) registers)]
          [:div.row {:key k}
           [:div.col-col-lg6.register-name {:key (str k "reg:name")} name]
           [:div.col-col-lg6.register-value {:id (str "reg" name) :key (str k "reg:value")} v]]))]]))

(defn cmp-values [cmp]
  (prn "cmp: " cmp)
  (cond (keyword-identical? cmp :lt) "<"
        (keyword-identical? cmp :gt) ">"
        (keyword-identical? cmp :eq) "="
        :else cmp))

;; Display the internal registers.
(defn internal-registers []
  (let [internal-registers @(rf/subscribe [:internal-registers])]
    [:div.registers-container
     [:div.registers-header "Internal Registers"]
     [:div.registers-list
      (when (not= internal-registers {})
        (for [[reg v] internal-registers]
          [:div.row
           [:div.col-col-lg6.register-name reg]
           [:div.col-col-lg6.register-value
            (cmp-values v)]]))]]))

;; Display the stack.
(defn stack [stack title]
  (let [stack @(rf/subscribe [stack])]
    [:div.stack-container
     [:div.stack-header title]
     [:div.stack-list
      (when (not= stack {})
        (for [r (reverse stack)]
          [:div.row
           [:div.col-col-lg6.stack-value r]]))]]))

;; Display the symbol table.
(defn symbol-table []
  (let [symbols @(rf/subscribe [:symbols])]
    [:div.symbol-table-container
     [:div.symbol-table-header "Symbol Table"]
     [:div.symbol-table
      (when (not= {} symbols)
        (for [s symbols]
          [:div.row
           [:div.col-col-lg6.symbol-name (key s)]
           [:div.col-col-lg6.symbol-value (val s)]]))]]))

;; -- App ---------------------------------------------------------------------------
(defn app []
  [:div.content
   [:div.row
    [:div.col.col-lg-4
     [code-editor]]
    [:div.col.col-lg-4
     [code]]
    [:div.col.col-lg-4]]
   [:div.row
    [:button.btn.btn-primary.parse-btn {:on-click #(rf/dispatch [:parse])} "Parse"]
    [:button.btn.btn-danger.parse-btn {:on-click #(rf/dispatch [:clear-parsed])} "Clear Parsed"]]
   [execution-controls]
   [:div.row.eip-container
    [eip]]
   [:div.grid
    [:div
     [registers]]
    [:div
     [internal-registers]]
    [:div
     [stack :stack "Stack"]]
    [:div
     [stack :eip-stack "EIP Stack"]]
    [:div
     [symbol-table]]]])

;; -- Dev Helpers -------------------------------------------------------------------
(comment (rf/dispatch-sync [:initialize]))
(comment (rf/dispatch-sync [:test-code]))
(comment (rf/dispatch-sync [:parse]))
(comment (rf/dispatch-sync [:toggle-breakpoint 11]))
(comment (rf/dispatch-sync [:reset-eip]))

(comment 
  (let [registers [[:a 1] [:b 2] [:c 3] [:d 4] [:e 5] [:f 6]]]
    ((doseq [r registers]
       (rf/dispatch-sync [:add-value-to-registers r])))))

(comment (rf/dispatch [:add-value-to-stack 4]))

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