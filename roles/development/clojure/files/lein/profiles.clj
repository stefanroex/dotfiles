{:user {:plugins [[lein-ancient "0.6.15"]
                  [jonase/eastwood "0.3.8"]
                  [lein-ns-dep-graph "0.2.0-SNAPSHOT"]
                  [lein-nvd "1.2.0"]
                  [lein-pprint "1.2.0"]
                  [ns-graph "0.1.3"]
                  [refactor-nrepl "2.4.0"]
                  [cider/cider-nrepl "0.22.4"]]
        :jvm-opts ["-Djdk.attach.allowAttachSelf"]
        :middleware [refactor-nrepl.plugin/middleware
                     cider-nrepl.plugin/middleware]
        :injections [(require 'sc.api)
                     (require 'clojure.pprint)]
        :dependencies [[vvvvalvalval/scope-capture "0.3.2"]
                       [com.clojure-goes-fast/clj-async-profiler "0.4.0"]
                       [com.clojure-goes-fast/clj-memory-meter "0.1.2"]]}}
