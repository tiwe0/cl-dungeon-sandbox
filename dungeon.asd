(asdf:defsystem "dungeon"
  :description "a render framework designed for old school roguelike game"
  :author "Ivory"
  :license "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (:sdl2 :sdl2-image :sdl2-ttf :3d-vectors)
  :components ((:file "src/globals")
               (:file "src/timer")
               (:file "src/color")
               (:file "src/glyph")
               (:file "src/gamemap")
               (:file "src/font")
               (:file "src/text")
               (:file "src/camera")
               (:file "src/twener")
               (:file "src/inputhandler")
               (:file "src/main")))

(asdf:defsystem "dungeon/test"
  :description "test"
  :author "Ivory"
  :license "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (:dungeon)
  :components ((:file "test/main")))
