ASSET_DIRS = $(shell find assets/ -type d)
ASSET_FILES = $(shell find assets/ -type f)
LW_PATH = $(shell which lw)

all: app.icns CJAWSAccess.app assets/accounts.json $(ASSET_DIRS) $(ASSET_FILES)
	rsync -arvh assets/ CJAWSAccess.app/Contents/Resources/
	touch CJAWSAccess.app

CJAWSAccess.app: deliver.lisp src/*.lisp aws-access.asd addFonts.patch
	/Applications/LispWorks\ 7.1\ \(64-bit\)/LispWorks\ \(64-bit\).app/Contents/MacOS/lispworks-7-1-0-amd64-darwin -build deliver.lisp
	patch -N -p0 < addFonts.patch

assets/accounts.json: accounts.yml
	./flip-yaml.lisp accounts.yml
	mv accounts.json assets/
	rm accounts.yml

accounts.yml:
	git archive --format=tar --remote=git@gitlab.cj.com:operations-chapter/aws-department.git heads/master -- accounts.yaml | tar xO accounts.yaml > accounts.yml

app.icns: icon/icon.svg
	$(MAKE) --directory=icon
	cp icon/icon.icns app.icns

deploy: all cleanDmg
	npx appdmg dmg.json CJAWSAccess.dmg

cleanDmg:
	rm -f CJAWSAccess.dmg

clean: cleanDmg
	rm -f assets/accounts.json
	rm -rf CJAWSAccess.app

dev:
	cp $(LW_PATH) CJAWSAccess.app/Contents/MacOS/lw
