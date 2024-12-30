# geepack v1.3.11 (2024-06-06)

	* Update of documentation of geeglm

# geepack v1.3.10 (2022-09-29)
	
	* Update of vignette.
	
# geepack v1.3.9 (2022-08-16)

	* QIC now takes environment argument because the function is called from another function in another package.
	
# geepack v1.3.8 (2022-08-12)

	* Version 1.3.7 never made it to CRAN for some reason (no changes of NEWS.md detected). Now NEWS.md has different markup.

# geepack v1.3.7 (2022-08-04)

	* Version 1.3.6 never made it to CRAN for some reason (no changes of NEWS.md detected). Now NEWS.md has different markup.
	
# geepack v1.3.6 (2022-07-23)

	* Version 1.3.5 never made it to CRAN for some reason (no changes of NEWS.md detected)

# geepack v1.3.5 (2022-07-18)

	* Fixed a critical error in QIC() (thanks to Brian McLoone and Steven Orzack for finding this)

# geepack v1.3.4 (2022-05-03)

    * Minor documentation update by request from CRAN

# geepack v1.3.3 (2022-01-04) 

	* Minor documentation updates
	* Minor polishing of code

# geepack v1.3.2 (Release date: 2020-12-18)

	* NAMESPACE and all .Rd files are now auto generated.
	* PROTECT / UNPROTECT issue fixed (in file inter.cc)
	* Improved documentation of dietox.


# geepack v1.3.1 (Release date: 2019-12-13)

	* PROTECT / UNPROTECT imbalance fixed
	* Version 1.3-1 uploaded

# geepack v1.3.0 (Release date: 2019-12-10)

	* Migrated to use roxygen
   * Improved documentation of geeglm
   * Check for data being sorted by 'id' i geeglm; a warning is issued if not.
   * QIC added; thanks to Claus Ekstrøm who is now a contributor.
   * tidy function from broom package is imported.
   * muscatine data added
   * Version 1.3-0 uploaded

# geepack v1.2.1 (Release date: 2014-09-13)

	* geeglm objects now inherits from lm also (to prevent warning when
		calling predict).

# geepack v1.2.0 (Release date: 2014-09-13)

  * Maintainer of geepack is now Søren Højsgaard
  * Location of vignette fixed
  * Version 1.2-0 uploaded
