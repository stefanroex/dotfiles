{:user {:plugins [[jonase/eastwood "0.2.5"]
                  [lein-ancient "0.6.14"]
                  [lein-cloverage "1.0.10"]
                  [lein-codox "0.10.3"]
                  [lein-pprint "1.1.2"]
                  [venantius/yagni "0.1.4"]
                  [lein-kibit "0.1.6"]]
        :dependencies [[pjstadig/humane-test-output "0.8.3"]]
        :injections [(require 'pjstadig.humane-test-output)
                     (pjstadig.humane-test-output/activate!)]}}
