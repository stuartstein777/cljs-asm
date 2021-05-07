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
      [:div.source-editor-header.header "Source Editor"
       [:button.btn.btn-danger.btn.py-0 {:on-click #(rf/dispatch [:clear-source])
                                         :style    {:font-size    "0.8em"
                                                    :float        :right
                                                    :margin-top   2
                                                    :margin-right 2}}
        "clear"]]
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
  (let [code            @(rf/subscribe [:code])
        breakpoints     @(rf/subscribe [:breakpoints])
        code-with-lines (zipmap (range (count code)) code)
        eip             @(rf/subscribe [:eip])
        on-breakpoint?  @(rf/subscribe [:on-breakpoint])]
    [:div {:style {:margin 10
                   :height 455
                   :overflow-y :none
                   :width  1000}}
     [:div.parsed-code-header.header
      [:div
       [:div {:style {:text-align   :left
                      :padding-left 5}}
        [:i.fas.fa-trash-alt {:style    {:color  :red
                                         :cursor :pointer}
                              :on-click #(rf/dispatch [:clear-breakpoints])}]
        [:label {:style {:margin-left 10}} "Parsed Code"]
        [:button.btn.btn-danger.btn.py-0 {:on-click #(rf/dispatch [:clear-parsed])
                                          :style    {:font-size    "0.8em"
                                                     :float        :right
                                                     :margin-top   2
                                                     :margin-right 2}}
         "clear"]]]]
     [:div#code-container.code-container
      [:table#code.code
       [:tbody
        (for [[line-no code-line] code-with-lines]
          [:tr.code-line {:key   line-no
                          :style {:background-color (if (= eip line-no) "goldenrod" "white")}}
           [:td.breakpoint
            [:i.fas.fa-circle {:style    {:color (if (some? (breakpoints line-no)) "red" "lightgray")}
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
               (for [i (h/keyed-collection arguments)]
                 (if (keyword? (val i))
                   [:label.register {:key (key i)} (val i)]
                   [:label.value {:key (key i)} (val i)])))]]])]]]
     [:div.breakpoint-indicator
      {:style {:visibility (if on-breakpoint? :visible :hidden)}}
      [:label (str "Breakpoint hit: Line " eip)]]]))

;; Buttons that control the executing code (start/stop/pause/speed)
(defn execution-controls []
  [:div.row
   (let [is-running? @(rf/subscribe [:running?])
         finished? @(rf/subscribe [:finished?])
         has-parsed-code? @(rf/subscribe [:has-parsed-code?])
         running-speed @(rf/subscribe [:running-speed])
         on-breakpoint @(rf/subscribe [:on-breakpoint])
         valid-running-speed (re-matches #"^\d+" running-speed)]
     [:div.execution-controls
      [:button.btn.btn-primary.parse-btn {:on-click #(rf/dispatch [:parse])} "Parse"]
      [:button.btn.btn-success.play-pause
       {:on-click #(rf/dispatch [:toggle-running])
        :disabled (and (or finished? (not has-parsed-code?) (not valid-running-speed)) (not on-breakpoint))}
       (if is-running? [:i.fas.fa-pause] [:i.fas.fa-play])]
      [:button.btn.btn-success.next-instruction
       {:on-click #(rf/dispatch [:next-instruction])
        :disabled (and (or finished? (not has-parsed-code?) is-running? ) (not on-breakpoint))}
       [:i.fas.fa-forward]]
      [:button.btn.btn-danger.stop-button
       {:on-click #(rf/dispatch [:reset])}
       (if finished? [:i.fas.fa-redo] [:i.fas.fa-stop])]
      [:input.instr-per-sec {:disabled    is-running?
                             :on-change   #(rf/dispatch-sync [:update-running-speed (-> % .-target .-value)])
                             :placeholder "1000"
                             :type        "text"
                             :value       running-speed}]
      [:label.speed-label "speed (msecs / instruction.)"]
      [:label.speed-value-error
       {:style {:visibility (if valid-running-speed :hidden :visible)}}
       "Speed should be a whole number."]])])

;; Display the current eip when running.
(defn eip []
  (let [eip @(rf/subscribe [:eip])]
    [:div
     [:label.eip-header.header "EIP"]
     [:label.eip eip]]))

;; Display the user registers.
(defn registers []
  (let [registers @(rf/subscribe [:registers])
        last-edit-register @(rf/subscribe [:last-edit-register])]
    [:div.registers-container
     [:div.registers-header.header "Registers"]
     [:div.registers-list
      (when (not= registers {})
        (for [[k [name v]] (h/keyed-collection registers)]
          [:div.row {:key k}
           [:div.col-col-lg6.register-name {:key (str k "reg:name")} name]
           (if (keyword-identical? name last-edit-register)
             [:div.col-col-lg6.register-value {:id (str "reg" name) :key (str k "reg:value")
                                               :style {:background-color :yellow}}
              v]
             [:div.col-col-lg6.register-value {:id (str "reg" name) :key (str k "reg:value")} v])]))]]))

(defn cmp-values [cmp]
  (cond (keyword-identical? cmp :lt) "<"
        (keyword-identical? cmp :gt) ">"
        (keyword-identical? cmp :eq) "="
        :else cmp))

;; Display the internal registers.
(defn internal-registers []
  (let [internal-registers (dissoc @(rf/subscribe [:internal-registers]) :errmsg)
        eip-stack @(rf/subscribe [:eip-stack])
        rep-counters @(rf/subscribe [:rep-counters-stack])]
    [:div.registers-container
     [:div.registers-header.header "Internal Registers"]
     [:div.registers-list
      (when (not= internal-registers {})
        (for [[k [reg v]] (h/keyed-collection internal-registers)]
          [:div.row {:key k}
           [:div.col-col-lg6.register-name {:key (str "irgn-" k)} reg]
           [:div.col-col-lg6.register-value {:key (str "irgv-" k)}
            (cmp-values v)]]))
      [:div
       [:div {:style {:float :left :width 103 :text-align :center}}
        [:div.header "EIP Stack"]
        [:div
         (for [[k r] (h/keyed-collection (reverse eip-stack))]
           [:div.eip-stack-value {:key k} r])]]
       [:div {:style {:float :right :width 103 :text-align :center}}
        [:div.header "RP Stack"]
        [:div 
         (for [[k r] (h/keyed-collection (reverse rep-counters))]
           [:div.eip-stack-value {:key k} r])]]]]]))

;; Display the stack.
(defn stack [stack title]
  (let [stack @(rf/subscribe [stack])]
    [:div.stack-container
     [:div.stack-header.header title]
     [:div.stack-list
      (when (not= stack {})
        (for [[k r] (h/keyed-collection (reverse stack))]
          [:div.row {:key k}
           [:div.col-col-lg6.stack-value {:key (str "stack-val-" k)} r]]))]]))

;; Display the symbol table.
(defn symbol-table []
  (let [symbols @(rf/subscribe [:symbols])]
    [:div.symbol-table-container
     [:div.symbol-table-header.header "Symbol Table"]
     [:div.symbol-table
      (when (not= {} symbols)
        (for [[k s] (h/keyed-collection symbols)]
          [:div.row {:key k}
           [:div.col-col-lg6.symbol-name {:key (str "symbols-name-" k)} (key s)]
           [:div.col-col-lg6.symbol-value {:key (str "symbols-value-" k)} (val s)]]))]]))

(defn supported-instructions []
  [:div
   [:h2 "Supported Instructions"]
   [:table.table.table-striped.table-hover {:style {:margin 10 :border "1px solid black"}}
    [:thead.table {:style {:background-color "rgb(18, 18, 19)" :color :white}}
     [:tr
      [:th {:style {:text-align :left :padding 10}} "Instruction"]
      [:th {:style {:text-align :left :padding 10}} "Example"]
      [:th {:style {:text-align :left :padding 10}} "Description"]]]
    [:tbody
     (let [instructions (h/get-supported-instructions)]
       (for [[k {:keys [instruction example description]}] (h/keyed-collection instructions)]
         [:tr {:key k :style {:border "1px solid black"}}
          [:td {:style {:width 150 :text-align :left :border-right "1px solid black"}} instruction]
          [:td {:style {:width 200 :text-align :left :border-right "1px solid black"}}
           example]
          [:td {:style {:width 500 :text-align :left}} description]]))]]])

(defn output []
  (let [output @(rf/subscribe [:output])]
    [:div.std-out-container
     [:div.header {:style {:text-align :left}}
      [:label {:style {:margin-left 5}}"Output"]
      [:button.btn.btn-danger.btn.py-0
       {:on-click #(rf/dispatch [:clear-output])
        :style    {:font-size    "0.8em"
                   :float        :right
                   :margin-top   2
                   :margin-right 2}}
       "clear"]]
     [:textarea.std-out {:value output
                         :readOnly  true
                         :wrap      :off}]]))

;; -- App ---------------------------------------------------------------------------
(defn app []
  [:div.content
   [:div.row
    [:div.col.col-lg-4
     [code-editor]]
    [:div.col.col-lg-4
     [code]]]
   [:div.row
    [execution-controls]]
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
     [symbol-table]]
    [:div
     [output]]]   
   [:div
    [supported-instructions]]])

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