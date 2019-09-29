CJAWSAccess.app: app.icns deliver.lisp src/*.lisp aws-access.asd accounts.json AuthorizeShell.scpt
	/Applications/LispWorks\ 7.1\ \(64-bit\)/LispWorks\ \(64-bit\).app/Contents/MacOS/lispworks-7-1-0-amd64-darwin -build deliver.lisp
	cp AuthorizeShell.scpt CJAWSAccess.app/Contents/Resources/
	cp accounts.json CJAWSAccess.app/Contents/Resources/accounts.json
	cp app.icns CJAWSAccess.app/Contents/Resources/app.icns
	touch CJAWSAccess.app

accounts.json: accounts.yml
	./flip-yaml.lisp accounts.yml

accounts.yml:
	git archive --format=tar --remote=git@gitlab.cj.com:operations-chapter/aws-department.git heads/master -- accounts.yaml | tar xO accounts.yaml > ~/accounts.yml

app.icns: icon/icon.svg
	$(MAKE) --directory=icon
	cp icon/icon.icns app.icns
