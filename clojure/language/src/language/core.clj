(ns language.core
  (:refer-clojure :exclude [eval]))

(declare eval-app)
(declare substitute)

(defn substitute-app
  [context [raw-f raw-arg]]
  (list (substitute context raw-f)
        (substitute context raw-arg)))

(defn substitute-fn
  [context [_fn [argument] body]]
  (list 'fn [argument] (substitute (dissoc context argument) body)))

(defn substitute
  [context body]
  (cond
    (symbol? body) (or (get context body) body)
    (integer? body) body
    (list? body) (if (= (first body) 'fn)
                     (substitute-fn context body)
                     (substitute-app context body))))

(defn eval-fn
  [context [_fn [argument] body]]
  (list 'fn [argument] (substitute (dissoc context argument) body)))

(defn eval
  ([x] (eval {} x))
  ([context x]
   (cond (integer? x) x
         (list? x) (if (= (first x) 'fn)
                     (eval-fn context x)
                     (eval-app context x))
         (symbol? x) (or (get context x)
                         (throw (ex-info (str "Unknown variable"
                                         {:symbol  x 
                                          :context context}) {}))))))

(defn eval-app
  [context [raw-f raw-arg]]
  (let [f (eval context raw-f)
        param (eval context raw-arg)
        [_fn [argument] body] f]
    (eval (assoc context argument param) body)))

