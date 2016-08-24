(ns tartan.core
  (:require [clojure.tools.cli :refer [parse-opts]])
  (:import java.awt.image.BufferedImage
           javax.imageio.ImageIO
           (java.awt Graphics2D RenderingHints Rectangle Color BasicStroke Font FontMetrics GradientPaint)
           (java.awt.geom Line2D$Double)
           (java.io File))
  (:gen-class))

(def colours {'K (Color. 16 16 16)
              'W (Color. 255 255 255)
              'R (Color. 255 0 0)
              'G (Color. 0 139 0)
              'B (Color. 0 0 205)
              'Y (Color. 255 230 0)
              'N (Color. 102 102 102)
              'T (Color. 96 51 17)})

(defn write-image
  "Write out the image to a png file.
   Call with e.g. (write-image \"C:\\Temp\\tartan.png\")."
  [bi image-path]
  (ImageIO/write bi "PNG" (File. image-path)))

(defn reverse-by-pairs
  "Takes a list and reverses order of pairs of adjacent elements"
  [lst]
  (flatten (reverse (partition 2 lst))))

(defn pre-process-sett
  "Check for repeating sett pattern and adjust if so."
  [sett]
  (if (= (second sett) '/)
    (let [start-colour (first sett)
          start-count (nth sett 2)
          end-colour (nth (reverse sett) 2)
          end-count (first (reverse sett))
          middle (drop-last 3 (drop 3 sett))]
      (flatten (list middle end-colour end-count (reverse-by-pairs middle) start-colour start-count)))
    sett))

(defn generate-threadcount
  "Create threadcount vector from sett."
  [sett]
  (loop [curr-sett (pre-process-sett sett) threadcount [] curr-colour 'K]
    (cond
      (empty? curr-sett)
        threadcount
      (number? (first curr-sett))
        (if (> (first curr-sett) 0)
          (recur (cons (dec (first curr-sett)) (rest curr-sett)) (conj threadcount curr-colour) curr-colour)
          (recur (rest curr-sett) threadcount curr-colour))
      :else
        (recur (rest curr-sett) threadcount (first curr-sett)))))
        

(defn calc-colour
  "Calculate correct colour for given coordinate."
  [threadcount x y]
  (if (or
        (and (odd? x) (odd? y))
        (and (even? x) (even? y)))
    (colours (threadcount x))
    (colours (threadcount y))))

(defn draw-tartan
  "Draw pixels on canvas based on the threadcount."
  [ig2 threadcount size reps]
  (dotimes [x (* size reps)]
    (dotimes [y (* size reps)]
      (let [r (Line2D$Double. x y x y)]
        (.setPaint ig2 (calc-colour threadcount (mod x size) (mod y size)))
        (.fill ig2 r)
        (.draw ig2 r)))))

(def cli-options
  [["-t" "--tartan TARTAN" "Tartan file"
    :default "tartan.trt"]
   ["-o" "--output FILE" "Output file"
    :default "tartan.png"]
   ["-r" "--reps REPS" "No. of pattern repetitions"
    :default 2
    :parse-fn #(Integer/parseInt %)]])

(defn -main
  "Render tartan sett from a text file."
  [& args]
  (let [{:keys [options]} (parse-opts args cli-options)
        sett (read-string (slurp (options :tartan)))
        threadcount (generate-threadcount (pre-process-sett sett))
        ;threadcount ['R 'R 'R 'R 'R 'R 'R 'R 'R 'R 'R 'R 'R 'R 'R 'R
        ;             'R 'R 'R 'R 'R 'R 'R 'R 'R 'R 'R 'R 'R 'R 'R 'R
        ;             'K 'K 'K 'K 'K 'K 'K 'K 'K 'K 'K 'K 'K 'K 'K 'K
        ;             'K 'K 'K 'K 'K 'K 'K 'K 'K 'K 'K 'K 'K 'K 'K 'K
        ;             'Y 'Y 'Y 'Y
        ;             'K 'K 'K 'K 'K 'K 'K 'K 'K 'K 'K 'K 'K 'K 'K 'K
        ;             'K 'K 'K 'K 'K 'K 'K 'K 'K 'K 'K 'K 'K 'K 'K 'K
        ;             'R 'R 'R 'R 'R 'R 'R 'R 'R 'R 'R 'R 'R 'R 'R 'R
        ;             'R 'R 'R 'R 'R 'R 'R 'R 'R 'R 'R 'R 'R 'R 'R 'R
        ;             'K 'K 'K 'K]
        size (- (count threadcount) 1)
        reps (options :reps)
        side-length (* size reps)
        x side-length
        y side-length
        r (Rectangle. 0 0 x y)
        bi (BufferedImage. x y BufferedImage/TYPE_INT_BGR)
        ig2 (.createGraphics bi)]

    ;Set graphics objects settings
    (.setRenderingHint
      ig2
      RenderingHints/KEY_ANTIALIASING
      RenderingHints/VALUE_ANTIALIAS_ON)

    ;Draw background rectangle
    (.setPaint ig2 (colours 'W))
    (.fill ig2 r)
    (.draw ig2 r)

    ;Render the tartan
    (draw-tartan ig2 threadcount size reps)

    ;Write out image to file
    (write-image bi (options :output))))
