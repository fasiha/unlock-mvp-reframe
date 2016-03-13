(ns mvp-reframe.handler
  (:import java.net.URLEncoder)
  (:require [compojure.core :refer [GET defroutes]]
            [compojure.route :as route]
            [ring.util.response :refer [file-response]]))

(defroutes handler
  (GET 
    "/parse/:text" 
    [text] 
    {
     :status 200
     :headers {"Content-Type" "application/json; charset=utf-8"}
     ; FIXME slurp here is synchronous right? But will JVM magic that away?
     :body (slurp (str "http://localhost:9090/" (URLEncoder/encode text)))})
  (GET "/foo" [] (do 
                   (println "In /foo!")
                   (str "duns")))
  (route/resources "/")
  (GET "/" [] (file-response "index.html" {:root "resources/public"}))
  (route/not-found "Not found.")
  )

