.PHONY: all docs

all: docs

docs:
	find src -iname '*.hs' |\
	xargs stack exec -- haddock \
	--hyperlinked-source \
	--html \
	-o docs \
	--optghc="-XMagicHash" \
	--optghc="-XMultiParamTypeClasses" \
	--optghc="-XOverloadedStrings" \
	--optghc="-XFlexibleInstances" \
	--optghc="-XFunctionalDependencies"

