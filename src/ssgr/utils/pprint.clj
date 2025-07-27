(ns ssgr.utils.pprint
  (:require [petitparser.core :as pp]
            [ssgr.utils.writer :as w]))

(defprotocol ParserPrinter
  (pretty-print! [parser writer]))

(extend-type petitparser.parsers.LiteralObjectParser
  ParserPrinter
  (pretty-print! [parser writer]
    (w/append! writer (pr-str (.-literal parser)))))

(extend-type petitparser.parsers.LiteralSequenceParser
  ParserPrinter
  (pretty-print! [parser writer]
    (w/append! writer (pr-str (.-literal parser)))))

(extend-type petitparser.parsers.SequenceParser
  ParserPrinter
  (pretty-print! [parser writer]
    (w/append! writer "[")
    (doseq [[i p] (map-indexed vector (.-parsers parser))]
      (when (> i 0)
        (w/append! writer " "))
      (pretty-print! p writer))
    (w/append! writer "]")))

(extend-type petitparser.parsers.ChoiceParser
  ParserPrinter
  (pretty-print! [parser writer]
    (w/append! writer "(or")
    (doseq [p (.-parsers parser)]
      (w/append! writer " ")
      (pretty-print! p writer))
    (w/append! writer ")")))

(extend-type petitparser.parsers.FlattenParser
  ParserPrinter
  (pretty-print! [parser writer]
    (w/append! writer "(flatten ")
    (pretty-print! (.-parser parser) writer)
    (w/append! writer ")")))

(extend-type petitparser.parsers.AndParser
  ParserPrinter
  (pretty-print! [parser writer]
    (w/append! writer "(and ")
    (pretty-print! (.-parser parser) writer)
    (w/append! writer ")")))

(extend-type petitparser.parsers.EndParser
  ParserPrinter
  (pretty-print! [parser writer]
    (w/append! writer "(end ")
    (pretty-print! (.-parser parser) writer)
    (w/append! writer ")")))

(extend-type petitparser.parsers.RepeatingParser
  ParserPrinter
  (pretty-print! [parser writer]
    (let [min (.-min parser)
          max (.-max parser)]
      (cond
        (and (zero? min) (= pp/MAX_VALUE max))
        (do (w/append! writer "(star ")
            (pretty-print! (.-parser parser) writer)
            (w/append! writer ")"))

        (and (= 1 min) (= pp/MAX_VALUE max))
        (do (w/append! writer "(plus ")
            (pretty-print! (.-parser parser) writer)
            (w/append! writer ")"))

        (= pp/MAX_VALUE max)
        (do (w/append! writer "(min ")
            (pretty-print! (.-parser parser) writer)
            (w/append! writer " " min)
            (w/append! writer ")"))

        (zero? min)
        (do (w/append! writer "(max ")
            (pretty-print! (.-parser parser) writer)
            (w/append! writer " " max)
            (w/append! writer ")"))

        :else
        (do (w/append! writer "(times ")
            (pretty-print! (.-parser parser) writer)
            (w/append! writer (str " " min " " max))
            (w/append! writer ")"))))))

(extend-type petitparser.parsers.GreedyRepeatingParser
  ParserPrinter
  (pretty-print! [parser writer]
    (let [min (.-min parser)
          max (.-max parser)]
      (cond
        (and (zero? min) (= pp/MAX_VALUE max))
        (do (w/append! writer "(star-greedy ")
            (pretty-print! (.-parser parser) writer)
            (w/append! writer " ")
            (pretty-print! (.-limit parser) writer)
            (w/append! writer ")"))

        (and (= 1 min) (= pp/MAX_VALUE max))
        (do (w/append! writer "(plus-greedy ")
            (pretty-print! (.-parser parser) writer)
            (w/append! writer " ")
            (pretty-print! (.-limit parser) writer)
            (w/append! writer ")"))

        (= pp/MAX_VALUE max)
        (do (w/append! writer "(min-greedy ")
            (pretty-print! (.-parser parser) writer)
            (w/append! writer " " min " ")
            (pretty-print! (.-limit parser) writer)
            (w/append! writer ")"))

        (zero? min)
        (do (w/append! writer "(max-greedy ")
            (pretty-print! (.-parser parser) writer)
            (w/append! writer " " max)
            (w/append! writer " ")
            (pretty-print! (.-limit parser) writer)
            (w/append! writer ")"))

        :else
        (do (w/append! writer "(GreedyRepeatingParser. ")
            (pretty-print! (.-parser parser) writer)
            (w/append! writer (str " " min " " max))
            (w/append! writer " ")
            (pretty-print! (.-limit parser) writer)
            (w/append! writer ")"))))))

(extend-type petitparser.parsers.LazyRepeatingParser
  ParserPrinter
  (pretty-print! [parser writer]
    (let [min (.-min parser)
          max (.-max parser)]
      (cond
        (and (zero? min) (= pp/MAX_VALUE max))
        (do (w/append! writer "(star-lazy ")
            (pretty-print! (.-parser parser) writer)
            (w/append! writer " ")
            (pretty-print! (.-limit parser) writer)
            (w/append! writer ")"))

        (and (= 1 min) (= pp/MAX_VALUE max))
        (do (w/append! writer "(plus-lazy ")
            (pretty-print! (.-parser parser) writer)
            (w/append! writer " ")
            (pretty-print! (.-limit parser) writer)
            (w/append! writer ")"))

        (= pp/MAX_VALUE max)
        (do (w/append! writer "(min-lazy ")
            (pretty-print! (.-parser parser) writer)
            (w/append! writer " " min " ")
            (pretty-print! (.-limit parser) writer)
            (w/append! writer ")"))

        (zero? min)
        (do (w/append! writer "(max-lazy ")
            (pretty-print! (.-parser parser) writer)
            (w/append! writer " " max)
            (w/append! writer " ")
            (pretty-print! (.-limit parser) writer)
            (w/append! writer ")"))

        :else
        (do (w/append! writer "(LazyRepeatingParser. ")
            (pretty-print! (.-parser parser) writer)
            (w/append! writer (str " " min " " max))
            (w/append! writer " ")
            (pretty-print! (.-limit parser) writer)
            (w/append! writer ")"))))))

(extend-type petitparser.parsers.NotParser
  ParserPrinter
  (pretty-print! [parser writer]
    (w/append! writer "(not ")
    (pretty-print! (.-parser parser) writer)
    (w/append! writer ")")))

(extend-type petitparser.parsers.OptionalParser
  ParserPrinter
  (pretty-print! [parser writer]
    (w/append! writer "(optional ")
    (pretty-print! (.-parser parser) writer)
    (w/append! writer ")")))

(extend-type petitparser.parsers.TokenParser
  ParserPrinter
  (pretty-print! [parser writer]
    (w/append! writer "(token ")
    (pretty-print! (.-parser parser) writer)
    (w/append! writer ")")))

(extend-type petitparser.parsers.ActionParser
  ParserPrinter
  (pretty-print! [parser writer]
    (w/append! writer "(transform ")
    (pretty-print! (.-parser parser) writer)
    (w/append! writer " " (pr-str (.-function parser)))
    (w/append! writer ")")))

(extend-type petitparser.parsers.TrimmingParser
  ParserPrinter
  (pretty-print! [parser writer]
    (w/append! writer "(trim ")
    (pretty-print! (.-parser parser) writer)
    (when-not (= pp/space (.-trimmer parser))
      (w/append! writer " ")
      (pretty-print! (.-trimmer parser) writer))
    (w/append! writer ")")))

(extend-type petitparser.parsers.PredicateObjectParser
  ParserPrinter
  (pretty-print! [parser writer]
    (condp = parser
      pp/any (w/append! writer "any")
      pp/digit (w/append! writer "digit")
      pp/letter (w/append! writer "letter")
      pp/word (w/append! writer "word")
      pp/space (w/append! writer "space")
      (doto writer
        (w/append! "(predicate ")
        (w/append! (pr-str (.-function parser)))
        (w/append! " ")
        (w/append! (pr-str (.-message parser)))
        (w/append! ")")))))

(extend-type petitparser.parsers.PredicateSequenceParser
  ParserPrinter
  (pretty-print! [parser writer]
    (w/append! writer "(predicate-sequence ")
    (w/append! writer
               (pr-str (.-function parser))
               " " (pr-str (.-message parser))
               " " (.-count parser))
    (w/append! writer ")")))

(extend-type petitparser.parsers.PlaceholderParser
  ParserPrinter
  (pretty-print! [parser writer]
    (w/append! writer (.-key parser))))

(extend-type petitparser.parsers.DelegateParser
  ParserPrinter
  (pretty-print! [parser writer]
    (w/append! writer "(delegate ")
    (pretty-print! @(.-parser parser) writer)
    (w/append! writer ")")))

(extend-type petitparser.parsers.CompositeParser
  ParserPrinter
  (pretty-print! [parser writer]
    (w/append! writer "(compose {")
    (doseq [[k v] (.-parsers parser)]
      (w/append! writer k)
      (w/append! writer " ")
      (pretty-print! v writer))
    (w/append! writer "})")))

(defn pprint-str [parser]
  (let [writer (w/make-writer)]
    (pretty-print! parser writer)
    (w/contents writer)))

(defn pprintln [parser]
  (println (pprint-str parser)))