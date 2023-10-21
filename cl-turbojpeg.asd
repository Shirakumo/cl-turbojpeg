(asdf:defsystem cl-turbojpeg
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "An up-to-date bindings library for the JPEG Turbo C library"
  :homepage "https://shirakumo.github.io/cl-turbojpeg/"
  :bug-tracker "https://github.com/shirakumo/cl-turbojpeg/issues"
  :source-control (:git "https://github.com/shirakumo/cl-turbojpeg.git")
  :serial T
  :components ((:file "package")
               (:file "turbojpeg")
               (:file "jpeg")
               (:file "wrapper")
               (:file "documentation"))
  :depends-on (:documentation-utils
               :cffi))
