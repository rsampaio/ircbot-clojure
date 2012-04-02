(ns ircbot-clj.core
  (:import (java.net Socket)
           (java.io PrintWriter InputStreamReader BufferedReader))
  (:gen-class))

(defstruct server-struct :name :port)
(defstruct nick-struct :name :nick)
(declare conn-handler)

(defn connect [server]
  (println (:name server) (:port server))
  (let [sock (Socket. (:name server) (:port server))
        in (BufferedReader. (InputStreamReader. (.getInputStream sock)))
        out (PrintWriter. (.getOutputStream sock))
        conn (ref {:in in :out out})]
      (doto (Thread. #(conn-handler conn)) (.start))
      conn))

(defn writer [conn msg]
  (doto (:out @conn) 
    (.println (str msg "\r")) 
    (.flush)))

(defn conn-handler [conn]
  (while (nil? (:exit @conn))
    (let [msg (.readLine (:in @conn))]
      (println msg)
      (cond 
        (let [[_ from priv chan cmd] (re-find #":(.*)!~.* (PRIVMSG) (.*) :(.*)" msg)]
          (if (not (nil? cmd))
            (def parsed_cmd {:from from :cmd cmd :chan chan}))
          parsed_cmd)
          (if (= (:cmd parsed_cmd) "!oi") 
            (writer conn (str "PRIVMSG " (:chan parsed_cmd) " :foda-se")))
        )
      (cond
        (re-find #"^ERROR :Closing Link:" msg) 
        (dosync (alter conn merge {:exit true}))
        (re-find #"^PING" msg)
        (writer conn (str "PONG " (re-find #":.*" msg))))
      )
    )
  )

(defn login [conn nick]
  (writer conn (str "NICK " (:name nick)))
  (writer conn (str "USER " (:nick nick) " 0 * : " (:name nick)))
  )

(defn -main [& args]
  (def nick (struct nick-struct "bot4fun" "bot4fun"))
  (def server (struct server-struct (.get args 0) (read-string (.get args 1))))
  (def irc (connect server))
  (login irc nick)
  (writer irc "JOIN #bot4fun")
  ;(writer irc "QUIT")
  )
