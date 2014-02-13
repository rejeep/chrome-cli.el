(ert-deftest chrome-cli-windows-test ()
  (with-mock
   (stub chrome-cli--command =>
         '("[1641] Tuxicity - Blog" "[38] rejeep/chrome-cli.el"))
   (should (equal (chrome-cli-windows)
                  '((:id "38" :name "rejeep/chrome-cli.el")
                    (:id "1641" :name "Tuxicity - Blog"))))))
