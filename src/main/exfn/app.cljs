(ns exfn.app
  (:require
   [reagent.dom :as dom]
   [re-frame-flow.core :as re-flow]
   [exfn.subs]
   [exfn.events]
   [exfn.helpers :as h]
   [re-frame.core :as rf]))

;; DEV NOTES
;; npx shadow-cljs watch app
;; then open web page in browser
;; then connect to repl.

;; -- Reagent Components ------------------------------------------------------------

;; Header with right aligned clear button.
(defn header-with-clear [clear-event title]
  [:div.parsed-code-header.header
   {:style {:text-align   :left
            :padding-left 5
            :padding-top 3}}
   title
   [:button.btn.btn-danger.btn.py-0
    {:on-click #(rf/dispatch [clear-event])
     :style    {:font-size    "0.8em"
                :float        :right
                :margin-top   2
                :margin-right 5}}
    "clear"]])

;; Source Code Editor.
(defn code-editor []
  (let [source @(rf/subscribe [:source])]
    [:div
     [:div.editor
      [header-with-clear :clear-source "Source Editor"]
      [:textarea#lineNos.text-editor-line-nos {:readOnly  true
                                               :value     (h/get-source-line-numbers source)}]
      [:textarea#editor.text-editor {:on-change  #(rf/dispatch-sync [:update-source (-> % .-target .-value)])
                                     :on-scroll  (fn [^js e]
                                                   (let [scroll-pos (.. e -target -scrollTop)]
                                                     (rf/dispatch [:update-scroll scroll-pos])))
                                     :spellCheck "false"
                                     :value      @(rf/subscribe [:source])
                                     :wrap       :off}]]]))

;; Display any parse errors.
(defn parse-errors []
  (let [errors @(rf/subscribe [:parse-errors])]
    [:div.parsed-code-container
     [:div.parsed-code-header.header
      [:div {:style {:text-align   :left
                     :padding-left 5
                     :padding-top 3}}
       "Errors"]]
     [:textarea#errors.parse-errors
      {:readOnly true
       :value errors
       :wrap      :off}]]))

;; Display the parsed code.
(defn code []
  (let [code            @(rf/subscribe [:code])
        breakpoints     @(rf/subscribe [:breakpoints])
        code-with-lines (mapv (fn [n l] [n l]) (iterate inc 1) code)
        eip             @(rf/subscribe [:eip])
        on-breakpoint?  @(rf/subscribe [:on-breakpoint])]
    [:div.parsed-code-container
     [:div.parsed-code-header.header
      [:div
       [:div {:style {:text-align   :left
                      :padding-left 5}}
        [:i.fas.fa-trash-alt {:style    {:color  :red
                                         :cursor :pointer
                                         :font-size 17
                                         :padding-top 3}
                              :on-click #(rf/dispatch [:clear-breakpoints])}]
        [:label {:style {:margin-left 10}} "Parsed Code"]
        [:button.btn.btn-danger.btn.py-0
         {:on-click #(rf/dispatch [:clear-parsed])
          :style    {:font-size    "0.8em"
                    :float        :right
                    :margin-top   5
                    :margin-right 5}}
         "clear"]]]]
     [:div#code-container.code-container
      [:table#code.code
       [:tbody
        (for [[line-no code-line] code-with-lines]
          [:tr.code-line {:key   line-no
                          :style {:background-color (if (= eip line-no) "goldenrod" "white")}}
           [:td.breakpoint
            [:i.fas.fa-circle {:style    {:color (if (some? (breakpoints line-no)) "red" "white")}
                               :on-click #(rf/dispatch [:toggle-breakpoint line-no])}]]
           [:td.code-eip
            [:i.fas.fa-angle-double-right
             {:style {:visibility (if (= eip line-no) :visible :hidden)}}]]
           [:td.line-numbers [:div {:style {:height 20}}
                             line-no]]
           [:td
            [:span
             [:label.instruction (first code-line)]
             (let [arguments (rest code-line)]
               (for [[k i] (h/keyed-collection arguments)]
                 (if (keyword? i)
                   [:label.register {:key k} i]
                   [:label.value {:key k} i])))]]])]]]
     [:div.breakpoint-indicator
      {:style {:visibility (if on-breakpoint? :visible :hidden)}}
      [:label (str "Breakpoint hit. Line: " eip)]]]))

;; Buttons that control the executing code (start/stop/pause/speed)
(defn execution-controls []
  [:div.row
   (let [is-running? @(rf/subscribe [:running?])
         finished? @(rf/subscribe [:finished?])
         has-parsed-code? @(rf/subscribe [:has-parsed-code?])
         running-speed @(rf/subscribe [:running-speed])
         on-breakpoint @(rf/subscribe [:on-breakpoint])
         valid-running-speed (re-matches #"^\d+" (str running-speed))
         waiting-on-input? @(rf/subscribe [:waiting-on-input?])]
     [:div.execution-controls
      [:button.btn.btn-primary.parse-btn {:on-click #(rf/dispatch [:parse])} "Parse"]
      [:button.btn.btn-success.play-pause
       {:on-click #(rf/dispatch [:toggle-running])
        :disabled (and (or finished? (not has-parsed-code?) (not valid-running-speed) waiting-on-input?) (not on-breakpoint))}
       (if is-running? [:i.fas.fa-pause] [:i.fas.fa-play])]
      [:button.btn.btn-success.next-instruction
       {:on-click #(rf/dispatch [:next-instruction])
        :disabled (and (or finished? (not has-parsed-code?) is-running? waiting-on-input?) (not on-breakpoint))}
       [:i.fas.fa-forward]]
      [:button.btn.btn-danger.stop-button
       {:disabled (not has-parsed-code?)
        :on-click #(rf/dispatch [:reset])}
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

;; An expandable box. Displays a configurable number of characters max.
;; Provides a .. to click to expand it, opens up into a bigger text box (higher, with horizontal / vertical scrollbar)
(defn expandable-box [name s last expanded?]
  [:div.expandable-box
   (if expanded?
     ; display full
     [:div {:style {:height "100%"}}
      [:div {:style {:background-color (if last :yellow :white)
                     :border-left "1px solid black"
                     :border-right "1px solid black"
                     :height 15}}
       [:i.fas.fa-compress-arrows-alt.expandable-box-minimise-icon
        {:on-click #(rf/dispatch [:remove-expanded-reg name])}]]
      [:textarea.expandable-text-area {:readOnly true
                                       :style    {:background-color (if last :yellow :white)}
                                       :value    s}]]
     ; display collapsed
     [:div.collapsed-expandable
      {:style {:background-color (if last :yellow :white)}}
      [:label (let [shortened (subs s 0 55)]
                (if (> (count s) (count shortened))
                  (str shortened "...")
                  shortened
                  ))]
      [:i.fas.fa-expand.expandable-expand-icon
       {:on-click #(rf/dispatch [:add-expanded-reg name])}]])])

;; Display the user registers.
(defn registers []
  (let [registers @(rf/subscribe [:registers])
        last-edit-register @(rf/subscribe [:last-edit-register])
        expanded-registers @(rf/subscribe [:expanded-registers])]
    [:div.registers-container
     [:div.registers-header.header "Registers"]
     [:div.registers-list
      (when (not= registers {})
        (for [[k [name v]] (h/keyed-collection registers)]
          [:div.row {:key k}
           [:div.col-col-lg6.register-name
            {:key (str k "reg:name")
             :style {:height (if (expanded-registers name) 70 20)}}
            name]
           [expandable-box name (str v) (keyword-identical? name last-edit-register) (expanded-registers name)]]))]]))

(defn cmp-values [cmp]
  (cond (keyword-identical? cmp :lt) "<"
        (keyword-identical? cmp :gt) ">"
        (keyword-identical? cmp :eq) "="
        :else cmp))

;; Display the internal registers.
(defn internal-registers []
  (let [internal-registers (dissoc @(rf/subscribe [:internal-registers]) :err-msg)
        eip-stack @(rf/subscribe [:eip-stack])
        rep-counters @(rf/subscribe [:rep-counters-stack])]
    [:div.internal-registers-container
     [:div.internal-registers-header.header "Internal Registers"]
     [:div.internal-registers-inner-container
      [:div.internal-registers-list
       (when (not= internal-registers {})
         (for [[k [reg v]] (h/keyed-collection internal-registers)]
           [:div.row {:key (str "irrow-" k)}
            [:div.internal-register-name {:key (str "irgn-" k)} reg]
            [:div.internal-register-value {:key (str "irgv-" k)}
             (cmp-values v)]]))]
      [:div.internal-registers-stacks-container
       [:div.eip-stack-container
        [:div.header "EIP Stack"]
        [:div
         (when (seq eip-stack)
           (for [[k r] (h/keyed-collection (reverse eip-stack))]
             [:div.eip-stack-value {:key (str "eipsv-" k)} r]))]]
       [:div.rp-stack-container
        [:div.header "RP Stack"]
        [:div
         (when (seq rep-counters)
           (for [[k r] (h/keyed-collection (reverse rep-counters))]
             [:div.eip-stack-value {:key (str "rpsv-" k)} r]))]]]]]))

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

;; Display the generated std output from the interpreter.
(defn output []
  (let [input @(rf/subscribe [:input])
        output @(rf/subscribe [:output])
        waiting-on-input? @(rf/subscribe [:waiting-on-input?])]
    [:div.std-out-container
     [header-with-clear :clear-output "Output"]
     [:textarea#stdout.std-out {:readOnly true
                                :spellCheck "false"
                                :value    output
                                :wrap     :off}]
     [:div {:style {:background-color (if waiting-on-input? :yellow :grey)
                    :border "1px solid black"
                    :padding 5}}
      [:i.fas.fa-keyboard]
      [:input {:disabled (not waiting-on-input?)
               :on-change #(rf/dispatch [:update-input (-> % .-target .-value)])
               :style {:margin-left 5
                       :height 23
                       :width 180
                       :margin-right 5}             
               :value input}]
      [:button.btn.btn-success.py-0
      {:disabled (not waiting-on-input?)
       :on-click #(rf/dispatch [:enter-input :continue])
       :style {:font-size "0.7em"
               :margin-left "5px"
               :margin-right "5px"}} 
       [:i.fas.fa-play]]
      [:button.btn.btn-success.py-0
       {:disabled (not waiting-on-input?)
        :on-click #(rf/dispatch [:enter-input :next])
        :style {:font-size "0.7em"}}
       [:i.fas.fa-forward]]]]))

;; -- App ---------------------------------------------------------------------------
(defn app []
  (let [parse-errors? @(rf/subscribe [:parse-errors?])]
    [:div.container
     [:div.row
      [:div.col.col-lg-4
       [code-editor]]
      [:div.col.col-lg-5
       (if parse-errors? 
         [parse-errors]
         [code])]
      [:div.col.col-lg-3
       [output]]]
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
       [symbol-table]]]]))

;; -- Dev Helpers -------------------------------------------------------------------
(comment (rf/dispatch-sync [:initialize]))
(comment (rf/dispatch-sync [:test-code]))
(comment (rf/dispatch-sync [:parse]))
(comment (rf/dispatch-sync [:toggle-breakpoint 11]))
(comment (rf/dispatch-sync [:reset-eip]))
(comment (rf/dispatch-sync [:toggle-parse-errors]))

(comment 
  (let [registers [[:a 1] [:b 2] [:c 3] [:d 4] [:e 5] [:f 6]]]
    ((doseq [r registers]
       (rf/dispatch-sync [:add-value-to-registers r])))))

(comment (rf/dispatch [:add-value-to-stack "foooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo"]))

;; -- After-Load --------------------------------------------------------------------
;; Do this after the page has loaded.
;; Initialize the initial db state.
(defn ^:dev/after-load start
  []
  (re-flow/clear-cache!)
  (dom/render [app] (.getElementById js/document "app")))

(defn ^:export init []
  (start))

(defonce initialize (rf/dispatch-sync [:initialize]))       ; dispatch the event which will create the initial state. 