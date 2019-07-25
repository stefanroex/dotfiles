{:user {:plugins [[lein-ancient "0.6.14"]
                  [lein-pprint "1.1.2"]
                  [lein-nvd "0.5.6"]
                  [cider/cider-nrepl "0.21.1"]
                  [refactor-nrepl "2.4.0"]]
        :jvm-opts ["-Djdk.attach.allowAttachSelf"]
        :middleware [refactor-nrepl.plugin/middleware
                     cider-nrepl.plugin/middleware]
        :injections [(require 'sc.api)
                     (require 'clojure.pprint)]
        :dependencies [[vvvvalvalval/scope-capture "0.3.2"]
                       [com.clojure-goes-fast/clj-async-profiler "0.3.0"]
                       [com.clojure-goes-fast/clj-memory-meter "0.1.2"]]}}
