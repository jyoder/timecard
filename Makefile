ifneq ($(wildcard IHP/.*),)
IHP = IHP/lib/IHP
else
IHP = $(shell dirname $$(which RunDevServer))/../lib/IHP
endif

CSS_FILES += ${IHP}/static/vendor/bootstrap.min.css
CSS_FILES += ${IHP}/static/vendor/flatpickr.min.css
CSS_FILES += static/app.css

JS_FILES += ${IHP}/static/vendor/jquery-3.2.1.slim.min.js
JS_FILES += ${IHP}/static/vendor/timeago.js
JS_FILES += ${IHP}/static/vendor/popper.min.js
JS_FILES += ${IHP}/static/vendor/bootstrap.min.js
JS_FILES += ${IHP}/static/vendor/flatpickr.js
JS_FILES += ${IHP}/static/helpers.js
JS_FILES += ${IHP}/static/vendor/morphdom-umd.min.js
JS_FILES += ${IHP}/static/vendor/turbolinks.js
JS_FILES += ${IHP}/static/vendor/turbolinksInstantClick.js
JS_FILES += ${IHP}/static/vendor/turbolinksMorphdom.js

include ${IHP}/Makefile.dist


.PHONY: tests pure-tests

pure-tests:
	ghcid --test=':main --skip "[IO]"' --command='ghci -itest Spec'

tests: export DATABASE_URL = postgresql:///test?host=$(PWD)/build/db

tests:
	dropdb -h $(PWD)/build/db --if-exists test
	createdb -h $(PWD)/build/db test
	psql -d $(DATABASE_URL) -f build/ihp-lib/IHPSchema.sql
	psql -d $(DATABASE_URL) -f Application/Schema.sql
	ghcid --test=':main' --command='ghci -itest Spec'
