(defproject io.simplect/compose			"0.6"
  :description					"Clojure library to aid composing functions"
  :license	{:name				"Eclipse Public License"
                 :url				"http://www.eclipse.org/legal/epl-v10.html"
                 :contact			"Klaus Harbo"
                 :email				"klaus@harbo.net"}
  :jar-name					"io.simplect.compose-%s.jar"
  :target-path					"target/%s"
  :profiles					{:uberjar {:aot :all}}
  :plugins					[[lein-tools-deps "0.4.1"]
                                                 [lein-codox "0.10.6"]]
  :middleware					[lein-tools-deps.plugin/resolve-dependencies-with-deps-edn]
  :scm						{:name "git", :url "https://github.com/simplect-io/io.simplect.compose"}
  :codox					{:source-paths ["src"]
                                                 :output-path "codox"
                                                 :metadata {:doc/format :markdown}
                                                 :namespaces [io.simplect.compose io.simplect.compose.notation]
                                                 :project {:name "io.simplect.compose"
                                                           :description "Clojure library to aid composing functions"}}
  :lein-tools-deps/config			{:config-files [:install :user :project]}
  )

