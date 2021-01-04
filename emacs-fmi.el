;;; emacs-fmi.el  --- Fetches weather data from opendata.fmi.fi
;;; Commentary:
;; This package fetches weather data from opendata.fmi.fi.
;;; Code:

(require 'dash)
(require 'url)
(require 'dom)

(defun fmi-station-id (station)
  "Return fmisid of the STATION."
  (-> (dom-by-tag station 'identifier)
      (dom-children)
      (car)))

(defun fmi-station-location (station)
  "Return location of the STATION."
  (-let (((lat lon) (-> (dom-by-tag station 'Point)
                        (dom-by-tag 'pos)
                        (dom-children)
                        (car)
                        (split-string))))
    `((lat . ,(string-to-number lat))
      (lon . ,(string-to-number lon)))))


(defun fmi-station-name (station)
  "Return name of the STATION."
  (-> (-first (lambda (name)
                (string=
                 "http://xml.fmi.fi/namespace/locationcode/name"
                 (dom-attr name 'codeSpace)))
              (dom-by-tag station 'name))
      (dom-children)
      (car)))

(defun square (x)
  "Return square of X."
  (* x x))

(defun fmi-sort-stations-by-proximity (bbox stations)
  "Sort STATIONS by their proximity to BBOX.
BBOX is an alist containing 'max-lat', 'min-lat', 'max-lon', 'min-lon'.
Seems to be most accurate for the nearest stations and lose
accuracy as distance to the station grows.  This is probably
because a simplified distance equation is used instead of
calculating the true distance between points."
  (sort
   (mapcar (lambda (station)
             (vector (fmi-station-location station)
                     (fmi-station-name station)
                     (fmi-station-id station)))
           stations)
   (lambda (s1 s2)
     (let* ((s1-pos (elt s1 0))
            (s2-pos (elt s2 0))
            (bbox-lat
             (/ (+ (alist-get 'max-lat bbox)
                   (alist-get 'min-lat bbox))
                2))
            (bbox-lon
             (/ (+ (alist-get 'max-lon bbox)
                   (alist-get 'min-lon bbox))
                2)))
       (< (+ (square (- (alist-get 'lat s1-pos) bbox-lat))
             (square (- (alist-get 'lon s1-pos) bbox-lon)))
          (+ (square (- (alist-get 'lat s2-pos) bbox-lat))
             (square (- (alist-get 'lon s2-pos) bbox-lon))))))))

(defun format-iso8601-time (time)
  "Return Emacs internal TIME formatted as ISO 8601 time."
  (format-time-string "%FT%T%z" time))

(defvar fmi-parameter-dictionary
  '(("TA_PT1H_AVG" . 'temperature)
    ("WS_PT1H_AVG" . 'wind-speed)
    ("RH_PT1H_AVG" . 'relative-humidity)
    ("PRA_PT1H_ACC" . 'precipitation)))

(defun fmi-read-parameter (feature)
  "Read parameters from FEATURE."
  (let ((pname  (-some-> (dom-by-tag feature 'ParameterName)
                  (dom-children)
                  (car)
                  (assoc fmi-parameter-dictionary)
                  (cdr)))
        (pval (-> (dom-by-tag feature 'ParameterValue)
                  (dom-children)
                  (car))))
    (when pname
      `(,pname . ,pval))))

(defun fmi-read-parameters (features)
  "Read parameters from FEATURES."
  (seq-reduce
   (lambda (acc feature)
     (let ((parameter (fmi-read-parameter feature)))
       (if parameter
           (cons parameter acc)
         acc)))
   features
   '()))

(defun fmi-query-wfs-features (parameters)
  "Query fmi WFS service with query PARAMETERS.
PARAMETERS is an alist.
Return parsed DOM of the response."
  (with-current-buffer
      (url-retrieve-synchronously
       (format
        "http://opendata.fmi.fi/wfs?service=WFS&version=2.0.0&request=getFeature&%s"
        (mapconcat (lambda (args)
                     (format "%s=%s"
                             (url-hexify-string (car args))
                             (url-hexify-string (cdr args))))
                   parameters
                   "&")))
    (goto-char 0)
    (forward-paragraph)
    (forward-char)
    (libxml-parse-xml-region (point) (point-max))))

(defun fmi-get-weather (fmisid start-time end-time)
  "Get weather for station with FMISID betweeb START-TIME and END-TIME.
FMSID as string, times as Emacs internal time."
  (-> (fmi-query-wfs-features
       `(("storedQuery" . "fmi::observations::weather::hourly::simple")
         ("fmisid" . ,fmisid)
         ("starttime" . ,(format-iso8601-time start-time))
         ("endtime" . ,(format-iso8601-time end-time))
         ("maxlocations" . "1")))

      (dom-by-tag 'member)
      (fmi-read-parameters)))

(defun fmi-get-weather-stations (network-id)
  "Get a list of weather stations in NETWORK-ID.
For example list Automatic Weather Stations using '121'."
  (fmi-query-wfs-features
   `(("storedquery_id" . "fmi::ef::stations")
    ("networkid" . ,network-id))))

(defun fmi-get-automatic-weather-stations ()
  "Get a list of automatic weather stations."
  (fmi-get-weather-stations "121"))

(defun fmi-get-weather-networks ()
  "Get a list of weather networks."
  (fmi-query-wfs-features
   '(("storedQuery" . "fmi::ef::networks"))))

;;; Stored queries available
;;; http://opendata.fmi.fi/wfs?service=WFS&version=2.0.0&request=describeStoredQueries&

;;; Describes parameter values
;;; https://opendata.fmi.fi/meta?observableProperty=observation&param=TA_PT1H_AVG&language=eng

(provide 'emacs-fmi)
;;; emacs-fmi.el ends here
