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

;; s 字符串
;; chr 注释字符
;; allow-anywhere? 注释是否可以在任何地方
;; 注释在任何地方可能的意思就是 值里面会出现;符号
(defn- strip-comment [s chr allow-anywhere?]
  ;; 取得注释的位置
  (let [n (.indexOf s (int chr))]
    ;; 处理位置 是否是小于0 是否可以任何地方都有注释
    (if (and (not (neg? n))
             (or allow-anywhere?
                 (zero? n)))
      ;; 截取注释之前的
      (subs s 0 n)
      ;; 整个字符串都返回
      s)))

;; (strip-comment "abc;eeee" \; true) "abc"
;; (strip-comment "eeee;aaaa" \; false) "eeee;aaaa"
;; (strip-comment ";abcdefgh" \; true) ""
;; (strip-comment ";abcdefgh" \; false) ”“

(defn- mapify [coll]
  (loop [xs coll m {} key nil]
    (if-let [x (first coll)]
      (if (vector? x)
        (if (nil? key)
          (recur (rest xs)
                 (assoc m (first x) (second x))
                 key)
          (recur (rest xs)
                 (assoc-in m [key (first x)]
                           (second x))
                 key))
        (recur (rest xs)
               (assoc m x {})
               x))
      m)))

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
  (let [kw (if keywordize? keyword identity)
        trim (if trim? s/trim identity)]
    (with-open [r (io/reader in)]
      (->> (line-seq r)
           (map #(strip-comment % comment-char allow-comments-anywhere?))
           (remove ())
           (map #(parse-line % kw trim))
           mapify)))
  )
