;; emeteo-stations.el --- Emeteo WebSources

;; $Id$
;; Author: Sebastian Freundt (hroptatyr@gna.org)
;; Keywords: metar, meteo, weather, emacs

;; Copyright (C) 2004  Sebastian Freundt

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;; Here we provide some data sources we've used to verify functionality of emeteo.
;; For more examples look for our .emeteo on the web, e.g.
;; * http://www.math.tu-berlin.de/~freundt/.emeteo

;; For more information, see the following URLs:
;; * http://sf.net/projects/emeteo/
;; * http://gna.org/projects/emeteo/

(setq emeteo-data-sources
      '((berlin
         :region-path (europe germany berlin)
         :uri "http://www.met.fu-berlin.de/de/wetter/"
         :uri-coordinates ((52 25) (13 13))
         :fallback ("http://weather.yahoo.com/forecast/GMXX0007.html")
         :fetch-chain default
         :converter-chain 'celsius
         :temp-unit celsius
         :temp-unit-string "°C"
         :wind-unit meterspersecond
         :wind-unit-string "m/s"
         :name "Berlin"
         :shortname "B")
        (reykjavik
         :region-path (europe iceland reykjavik)
         :uri "http://search.weather.yahoo.com/forecast/ICXX0002_c.html?force_units=1"
         :fallback ("http://www.theweathernetwork.com/weather/cities/intl/pages/ICXX0002.htm")
         :fetch-chain default
         :temp-unit-string "°C"
         :wind-unit-string "m/s"
         :name "Reykjavík"
         :shortname "R")
        (berlin-eddi
         :uri "http://weather.noaa.gov/weather/current/EDDI.html"
         :fetch-chain default
         :converter-chain '(fahrenheit celsius)
         :temp-unit celsius
         :temp-unit-string "°C"
         :wind-unit meterspersecond
         :wind-unit-string "m/s"
         :name "EDDI"
         :shortname "EDDI"))
      emeteo-modeline-eyecandy-p t
      emeteo-modeline-specifiers '(berlin reykjavik berlin-eddi))




(provide 'emeteo-stations)

;;; emeteo-stations.el ends here
