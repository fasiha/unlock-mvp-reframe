(ns mvp-reframe.handler
  (:import java.net.URLEncoder)
  (:require [compojure.core :refer [GET POST defroutes]]
            [compojure.route :as route]
            [ring.util.response :refer [file-response]]
            [ring.middleware.format :refer [wrap-restful-format]]
            [ring.middleware.reload :refer [wrap-reload]]
            [mvp-reframe.taggers :as taggers]))

(defroutes handler
  (POST
    "/jmdict"
    req
    (do (println "/jmdict requested lexeme:" (:params req))
        (let [lexeme (:params req)]
          {:body (taggers/lexeme-to-headwords lexeme)})))
  (GET
    "/parse/:text"
    [text]
    (do
      (println "Parsing text:" text)
      {
       :status 200
       :headers {"Content-Type" "application/json; charset=utf-8"}
       ; FIXME slurp here is synchronous right? But will JVM magic that away?
       :body (slurp (str "http://localhost:9090/" (URLEncoder/encode text)))}))
  (GET "/foo" [] (do
                   (println "In /foo!")
                   (str "FOO")))
  (route/resources "/")
  (GET "/" [] (file-response "index.html" {:root "resources/public"}))
  (route/not-found "Not found.")
  )

(def app (-> #'handler
             (wrap-reload)
             (wrap-restful-format)))
