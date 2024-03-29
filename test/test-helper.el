(require 'f)

(defvar chrome-cli-test/test-path
  (f-parent (f-this-file)))

(defvar chrome-cli-test/root-path
  (f-parent chrome-cli-test/test-path))

(require 'ert)
(require 'chrome-cli (f-expand "chrome-cli" chrome-cli-test/root-path))

(setq chrome-cli-executable (f-expand "chrome-cli" chrome-cli-test/test-path))
