(defpackage :dungeon/color
  (:use :cl :3d-vectors)
  (:export #:+maroon+
           #:+dark-red+
           #:+brown+
           #:+firebrick+
           #:+crimson+
           #:+red+
           #:+tomato+
           #:+coral+
           #:+indian-red+
           #:+light-coral+
           #:+dark-salmon+
           #:+salmon+
           #:+light-salmon+
           #:+orange-red+
           #:+dark-orange+
           #:+orange+
           #:+gold+
           #:+dark-golden-rod+
           #:+golden-rod+
           #:+pale-golden-rod+
           #:+dark-khaki+
           #:+khaki+
           #:+olive+
           #:+yellow+
           #:+yellow-green+
           #:+dark-olive-green+
           #:+olive-drab+
           #:+lawn-green+
           #:+chartreuse+
           #:+green-yellow+
           #:+dark-green+
           #:+green+
           #:+forest-green+
           #:+lime+
           #:+lime-green+
           #:+light-green+
           #:+pale-green+
           #:+dark-sea-green+
           #:+medium-spring-green+
           #:+spring-green+
           #:+sea-green+
           #:+medium-aqua-marine+
           #:+medium-sea-green+
           #:+light-sea-green+
           #:+dark-slate-gray+
           #:+teal+
           #:+dark-cyan+
           #:+aqua+
           #:+cyan+
           #:+light-cyan+
           #:+dark-turquoise+
           #:+turquoise+
           #:+medium-turquoise+
           #:+pale-turquoise+
           #:+aqua-marine+
           #:+powder-blue+
           #:+cadet-blue+
           #:+steel-blue+
           #:+corn-flower-blue+
           #:+deep-sky-blue+
           #:+dodger-blue+
           #:+light-blue+
           #:+sky-blue+
           #:+light-sky-blue+
           #:+midnight-blue+
           #:+navy+
           #:+dark-blue+
           #:+medium-blue+
           #:+blue+
           #:+royal-blue+
           #:+blue-violet+
           #:+indigo+
           #:+dark-slate-blue+
           #:+slate-blue+
           #:+medium-slate-blue+
           #:+medium-purple+
           #:+dark-magenta+
           #:+dark-violet+
           #:+dark-orchid+
           #:+medium-orchid+
           #:+purple+
           #:+thistle+
           #:+plum+
           #:+violet+
           #:+magenta+
           #:+orchid+
           #:+medium-violet-red+
           #:+pale-violet-red+
           #:+deep-pink+
           #:+hot-pink+
           #:+light-pink+
           #:+pink+
           #:+antique-white+
           #:+beige+
           #:+bisque+
           #:+blanched-almond+
           #:+wheat+
           #:+corn-silk+
           #:+lemon-chiffon+
           #:+light-golden-rod-yellow+
           #:+light-yellow+
           #:+saddle-brown+
           #:+sienna+
           #:+chocolate+
           #:+peru+
           #:+sandy-brown+
           #:+burly-wood+
           #:+tan+
           #:+rosy-brown+
           #:+moccasin+
           #:+navajo-white+
           #:+peach-puff+
           #:+misty-rose+
           #:+lavender-blush+
           #:+linen+
           #:+old-lace+
           #:+papaya-whip+
           #:+sea-shell+
           #:+mint-cream+
           #:+slate-gray+
           #:+light-slate-gray+
           #:+light-steel-blue+
           #:+lavender+
           #:+floral-white+
           #:+alice-blue+
           #:+ghost-white+
           #:+honeydew+
           #:+ivory+
           #:+azure+
           #:+snow+
           #:+black+
           #:+dim-gray+
           #:+gray+
           #:+dark-gray+
           #:+silver+
           #:+light-gray+
           #:+gainsboro+
           #:+white-smoke+
           #:+white+))
(in-package :dungeon/color)

;; r g b a
(defparameter +maroon+ (vec4 128 0 0 255))
(defparameter +dark-red+ (vec4 139 0 0 255))
(defparameter +brown+ (vec4 165 42 42 255))
(defparameter +firebrick+ (vec4 178 34 34 255))
(defparameter +crimson+ (vec4 220 20 60 255))
(defparameter +red+ (vec4 255 0 0 255))
(defparameter +tomato+ (vec4 255 99 71 255))
(defparameter +coral+ (vec4 255 127 80 255))
(defparameter +indian-red+ (vec4 205 92 92 255))
(defparameter +light-coral+ (vec4 240 128 128 255))
(defparameter +dark-salmon+ (vec4 233 150 122 255))
(defparameter +salmon+ (vec4 250 128 114 255))
(defparameter +light-salmon+ (vec4 255 160 122 255))
(defparameter +orange-red+ (vec4 255 69 0 255))
(defparameter +dark-orange+ (vec4 255 140 0 255))
(defparameter +orange+ (vec4 255 165 0 255))
(defparameter +gold+ (vec4 255 215 0 255))
(defparameter +dark-golden-rod+ (vec4 184 134 11 255))
(defparameter +golden-rod+ (vec4 218 165 32 255))
(defparameter +pale-golden-rod+ (vec4 238 232 170 255))
(defparameter +dark-khaki+ (vec4 189 183 107 255))
(defparameter +khaki+ (vec4 240 230 140 255))
(defparameter +olive+ (vec4 128 128 0 255))
(defparameter +yellow+ (vec4 255 255 0 255))
(defparameter +yellow-green+ (vec4 154 205 50 255))
(defparameter +dark-olive-green+ (vec4 85 107 47 255))
(defparameter +olive-drab+ (vec4 107 142 35 255))
(defparameter +lawn-green+ (vec4 124 252 0 255))
(defparameter +chartreuse+ (vec4 127 255 0 255))
(defparameter +green-yellow+ (vec4 173 255 47 255))
(defparameter +dark-green+ (vec4 0 100 0 255))
(defparameter +green+ (vec4 0 128 0 255))
(defparameter +forest-green+ (vec4 34 139 34 255))
(defparameter +lime+ (vec4 0 255 0 255))
(defparameter +lime-green+ (vec4 50 205 50 255))
(defparameter +light-green+ (vec4 144 238 144 255))
(defparameter +pale-green+ (vec4 152 251 152 255))
(defparameter +dark-sea-green+ (vec4 143 188 143 255))
(defparameter +medium-spring-green+ (vec4 0 250 154 255))
(defparameter +spring-green+ (vec4 0 255 127 255))
(defparameter +sea-green+ (vec4 46 139 87 255))
(defparameter +medium-aqua-marine+ (vec4 102 205 170 255))
(defparameter +medium-sea-green+ (vec4 60 179 113 255))
(defparameter +light-sea-green+ (vec4 32 178 170 255))
(defparameter +dark-slate-gray+ (vec4 47 79 79 255))
(defparameter +teal+ (vec4 0 128 128 255))
(defparameter +dark-cyan+ (vec4 0 139 139 255))
(defparameter +aqua+ (vec4 0 255 255 255))
(defparameter +cyan+ (vec4 0 255 255 255))
(defparameter +light-cyan+ (vec4 224 255 255 255))
(defparameter +dark-turquoise+ (vec4 0 206 209 255))
(defparameter +turquoise+ (vec4 64 224 208 255))
(defparameter +medium-turquoise+ (vec4 72 209 204 255))
(defparameter +pale-turquoise+ (vec4 175 238 238 255))
(defparameter +aqua-marine+ (vec4 127 255 212 255))
(defparameter +powder-blue+ (vec4 176 224 230 255))
(defparameter +cadet-blue+ (vec4 95 158 160 255))
(defparameter +steel-blue+ (vec4 70 130 180 255))
(defparameter +corn-flower-blue+ (vec4 100 149 237 255))
(defparameter +deep-sky-blue+ (vec4 0 191 255 255))
(defparameter +dodger-blue+ (vec4 30 144 255 255))
(defparameter +light-blue+ (vec4 173 216 230 255))
(defparameter +sky-blue+ (vec4 135 206 235 255))
(defparameter +light-sky-blue+ (vec4 135 206 250 255))
(defparameter +midnight-blue+ (vec4 25 25 112 255))
(defparameter +navy+ (vec4 0 0 128 255))
(defparameter +dark-blue+ (vec4 0 0 139 255))
(defparameter +medium-blue+ (vec4 0 0 205 255))
(defparameter +blue+ (vec4 0 0 255 255))
(defparameter +royal-blue+ (vec4 65 105 225 255))
(defparameter +blue-violet+ (vec4 138 43 226 255))
(defparameter +indigo+ (vec4 75 0 130 255))
(defparameter +dark-slate-blue+ (vec4 72 61 139 255))
(defparameter +slate-blue+ (vec4 106 90 205 255))
(defparameter +medium-slate-blue+ (vec4 123 104 238 255))
(defparameter +medium-purple+ (vec4 147 112 219 255))
(defparameter +dark-magenta+ (vec4 139 0 139 255))
(defparameter +dark-violet+ (vec4 148 0 211 255))
(defparameter +dark-orchid+ (vec4 153 50 204 255))
(defparameter +medium-orchid+ (vec4 186 85 211 255))
(defparameter +purple+ (vec4 128 0 128 255))
(defparameter +thistle+ (vec4 216 191 216 255))
(defparameter +plum+ (vec4 221 160 221 255))
(defparameter +violet+ (vec4 238 130 238 255))
(defparameter +magenta+ (vec4 255 0 255 255))
(defparameter +orchid+ (vec4 218 112 214 255))
(defparameter +medium-violet-red+ (vec4 199 21 133 255))
(defparameter +pale-violet-red+ (vec4 219 112 147 255))
(defparameter +deep-pink+ (vec4 255 20 147 255))
(defparameter +hot-pink+ (vec4 255 105 180 255))
(defparameter +light-pink+ (vec4 255 182 193 255))
(defparameter +pink+ (vec4 255 192 203 255))
(defparameter +antique-white+ (vec4 250 235 215 255))
(defparameter +beige+ (vec4 245 245 220 255))
(defparameter +bisque+ (vec4 255 228 196 255))
(defparameter +blanched-almond+ (vec4 255 235 205 255))
(defparameter +wheat+ (vec4 245 222 179 255))
(defparameter +corn-silk+ (vec4 255 248 220 255))
(defparameter +lemon-chiffon+ (vec4 255 250 205 255))
(defparameter +light-golden-rod-yellow+ (vec4 250 250 210 255))
(defparameter +light-yellow+ (vec4 255 255 224 255))
(defparameter +saddle-brown+ (vec4 139 69 19 255))
(defparameter +sienna+ (vec4 160 82 45 255))
(defparameter +chocolate+ (vec4 210 105 30 255))
(defparameter +peru+ (vec4 205 133 63 255))
(defparameter +sandy-brown+ (vec4 244 164 96 255))
(defparameter +burly-wood+ (vec4 222 184 135 255))
(defparameter +tan+ (vec4 210 180 140 255))
(defparameter +rosy-brown+ (vec4 188 143 143 255))
(defparameter +moccasin+ (vec4 255 228 181 255))
(defparameter +navajo-white+ (vec4 255 222 173 255))
(defparameter +peach-puff+ (vec4 255 218 185 255))
(defparameter +misty-rose+ (vec4 255 228 225 255))
(defparameter +lavender-blush+ (vec4 255 240 245 255))
(defparameter +linen+ (vec4 250 240 230 255))
(defparameter +old-lace+ (vec4 253 245 230 255))
(defparameter +papaya-whip+ (vec4 255 239 213 255))
(defparameter +sea-shell+ (vec4 255 245 238 255))
(defparameter +mint-cream+ (vec4 245 255 250 255))
(defparameter +slate-gray+ (vec4 112 128 144 255))
(defparameter +light-slate-gray+ (vec4 119 136 153 255))
(defparameter +light-steel-blue+ (vec4 176 196 222 255))
(defparameter +lavender+ (vec4 230 230 250 255))
(defparameter +floral-white+ (vec4 255 250 240 255))
(defparameter +alice-blue+ (vec4 240 248 255 255))
(defparameter +ghost-white+ (vec4 248 248 255 255))
(defparameter +honeydew+ (vec4 240 255 240 255))
(defparameter +ivory+ (vec4 255 255 240 255))
(defparameter +azure+ (vec4 240 255 255 255))
(defparameter +snow+ (vec4 255 250 250 255))
(defparameter +black+ (vec4 0 0 0 255))
(defparameter +dim-gray+ (vec4 105 105 105 255))
(defparameter +gray+ (vec4 128 128 128 255))
(defparameter +dark-gray+ (vec4 169 169 169 255))
(defparameter +silver+ (vec4 192 192 192 255))
(defparameter +light-gray+ (vec4 211 211 211 255))
(defparameter +gainsboro+ (vec4 220 220 220 255))
(defparameter +white-smoke+ (vec4 245 245 245 255))
(defparameter +white+ (vec4 255 255 255 255))
