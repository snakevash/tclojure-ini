(ns tclojure-ini.core
  (:gen-class)
  (:require [clojure.string :as s]
            [clojure.java.io :as io]))

(defn- parse-line [s kw trim]
  ;; 判断是否是sections
  (if (= (first s) \[)
    ;; 把sections转换成keyword
    (-> s
        (subs 1 (.indexOf s "]"))
        trim
        kw)
    ;; 取出前后的值
    (let [n (.indexOf s "=")]
      ;; 是否含有=
      (if (neg? n)
        ;; 没有= 那就说明格式出错了
        (throw (Exception. (str "Could not parse: " s)))
        ;; 取出=前后的值 以vector的形式
        [(-> s (subs 0 n) trim kw)
         (-> s (subs (inc n)) trim)]))))

;; 截取子字符串
;; (subs "clojure" 1 3) "lo"
;; (subs "clojure" 1) "lojure"

;; 去掉前后空格
;; (s/trim "   a   ") "a"

;; 转换成keyword
;; (keyword 'foo) :foo
;; (keyword "foo") :foo
;; (keyword "user" "foo") :user/foo
;; (keyword (str *ns*) "foo") :tclojure-ini.core/foo

;; 判断数字是否小于0
;; (neg? -1) true
;; (neg? 0) false
;; (neg? 1) false

(defn read-ini
  "
  读取一个.ini文件 转化到clojure map

  选择参数:

  "
  [in & {:keys [keywordize?
                trim?
                allow-comments-anywhere?
                comment-char]
         :or {keywordize? false
              trim? true
              allow-comments-anywhere? true
              comment-char \;}}]
  {:pre [(char? comment-char)]}
  (let )
  )
