(uiop:define-package #:commondoc-markdown/ci
  (:use #:cl)
  (:import-from #:40ants-ci/workflow
                #:defworkflow)
  (:import-from #:40ants-ci/jobs/linter)
  (:import-from #:40ants-ci/jobs/run-tests)
  (:import-from #:40ants-ci/jobs/docs))
(in-package #:commondoc-markdown/ci)


(defworkflow linter
  :on-pull-request t
  :cache t
  :jobs ((40ants-ci/jobs/linter:linter)))

;; There is no tests yet
;; (defworkflow ci
;;   :on-push-to "master"
;;   :by-cron "0 10 * * 1"
;;   :on-pull-request t
;;   :cache t
;;   :jobs ((40ants-ci/jobs/run-tests:run-tests
;;           :coverage t)))

(defworkflow docs
  :on-push-to "master"
  :on-pull-request t
  :cache t
  :jobs ((40ants-ci/jobs/docs:build-docs
          :asdf-system "commondoc-markdown-docs")))
