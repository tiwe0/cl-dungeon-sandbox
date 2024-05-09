(asdf:defsystem "dungeon"
  :description "Describe %ApplicationName% here"
  :author "complexitycollapse <complexitycollapse@github.com>"
  :license ""
  :version "0.0.1"
  :serial t
  :depends-on (:sdl2 :sdl2-image :sdl2-ttf :3d-vectors)
  :components ((:file "src/globals")
               (:file "src/font")
               (:file "src/glyph")
               (:file "src/camera")
               (:file "src/main")))

(asdf:defsystem "dungeon/test"
  :description "Describe %ApplicationName% here"
  :author "complexitycollapse <complexitycollapse@github.com>"
  :license ""
  :version "0.0.1"
  :serial t
  :depends-on (:dungeon)
  :components ((:file "test/main")))
