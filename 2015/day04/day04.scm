(load "../../lib/lib.scm")
(require openssl/md5)

(display (md5 (open-input-string "123456769)))
