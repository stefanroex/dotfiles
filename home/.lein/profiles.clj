{:user {:plugins [[lein-kibit "0.1.6"]
                  [lein-cloverage "1.0.10"]
                  [lein-ancient "0.6.14"]
                  [jonase/eastwood "0.2.5"]]
        :dependencies [[pjstadig/humane-test-output "0.8.3"]]
        :injections [(require 'pjstadig.humane-test-output)
                     (pjstadig.humane-test-output/activate!)]}}
