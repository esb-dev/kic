; kic Kodkod in Clojure -- Project definition

; Copyright (c) 2014 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(defproject kic "0.1.0-SNAPSHOT"
  :description "Kodkod in Clojure"
  :url "http://homepages.thm.de/~hg11260/kic.html"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [kodkod "2.0.0"]
                 [org.ow2.sat4j/org.ow2.sat4j.core "2.3.5"]]
  :plugins [[lein-marginalia "0.8.0"]
            [lein-localrepo "0.5.3"]]
)

; The Kodkod Java library in kodkod.jar is not available
; in a public Maven repository. To include it in the
; Leiningen dependnecies, it needs to be downloaded from
; http://alloy.mit.edu/kodkod/download.html and added to the
; local Maven repository with the help of the lein-localrepo
; plugin for Leiningen
