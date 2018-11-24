{:user {:plugins [[lein-ancient "0.6.14"]
                  [lein-pprint "1.1.2"]
                  [lein-nvd "0.5.6"]]
        :injections [(require 'sc.api)
                     (require 'clojure.pprint)]
        :dependencies [[vvvvalvalval/scope-capture "0.3.2"]
                       [com.clojure-goes-fast/clj-memory-meter "0.1.2"]]}}
